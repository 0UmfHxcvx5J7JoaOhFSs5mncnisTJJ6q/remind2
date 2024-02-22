#' Read in GDX and calculate final energy, used in convGDX2MIF.R for the
#' reporting
#'
#' Read in final energy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @author Renato Rodrigues, Felix Schreyer
#' @examples
#'
#'   \dontrun{reportFE(gdx)}
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass new.magpie mselect getRegions getYears mbind setNames
#'                      getNames<- as.data.frame as.magpie getSets
#' @importFrom dplyr %>% filter full_join group_by left_join mutate rename
#'     select semi_join summarise ungroup
#' @importFrom quitte inline.data.frame revalue.levels
#' @importFrom rlang syms
#' @importFrom tibble as_tibble tibble tribble
#' @importFrom tidyr complete crossing expand_grid replace_na
#' @importFrom utils tail

reportFE <- function(gdx, regionSubsetList = NULL,
                     t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130,
                           2150)) {
message('line #35')
  # conversion factors
  TWa_2_EJ     <- 31.536

message('line #39')
  out <- NULL

  ####### Core Variables ##########

  # ---- read in needed data

  # ---- sets
message('line #47')
  se2fe <- readGDX(gdx,"se2fe")
message('line #49')
  entyFe2Sector <- readGDX(gdx, "entyFe2Sector")
message('line #51')
  sector2emiMkt <- readGDX(gdx, "sector2emiMkt")

message('line #54')
  entyFe2sector2emiMkt_NonEn <- readGDX(gdx, "entyFe2sector2emiMkt_NonEn", react = "silent")
message('line #56')
  if (is.null(entySEfos <- readGDX(gdx, 'entySEfos', react = 'silent')))
    entySEfos <- c('sesofos', 'seliqfos', 'segafos')

message('line #60')
  if (is.null(entySEbio <- readGDX(gdx, 'entySEbio', react = 'silent')))
    entySEbio <- c('sesobio', 'seliqbio', 'segabio')

message('line #64')
  if (   is.null(entySEsyn <- readGDX(gdx, 'entySEsyn', react = 'silent'))
     || (length(entySEbio) == length(entySEsyn) && all(entySEbio == entySEsyn)))
    entySEsyn <- c('seliqsyn', 'segasyn')

message('line #69')
  demFemapping <- entyFe2Sector %>%
    full_join(sector2emiMkt, by = 'emi_sectors', relationship = "many-to-many") %>%
    # rename such that all_enty1 always signifies the FE carrier like in
    # vm_demFeSector
    rename(all_enty1 = 'all_enty') %>%
    left_join(se2fe, by = 'all_enty1', relationship = "many-to-many") %>%
    select(-'all_te')

  #sety <- readGDX(gdx,c("entySe","sety"),format="first_found")

  # ---- parameter
message('line #81')
  p_eta_conv <- readGDX(gdx, c("pm_eta_conv"), restore_zeros = FALSE,format="first_found")[,t,]

  # ---- variables
message('line #85')
  vm_prodSe <- readGDX(gdx,name=c("vm_prodSe","v_seprod"),field="l",restore_zeros=FALSE,format="first_found")[,t,]*TWa_2_EJ
  #vm_prodSe <- mselect(vm_prodSe,all_enty1=sety)
message('line #88')
  vm_prodFe  <- readGDX(gdx,name=c("vm_prodFe"),field="l",restore_zeros=FALSE,format="first_found")[,t,]*TWa_2_EJ
  #vm_prodFe  <- vm_prodFe[se2fe]
message('line #91')
  vm_demFeSector <- readGDX(gdx,name=c("vm_demFeSector"),field="l",format="first_found",restore_zeros=FALSE)[,t,]*TWa_2_EJ
message('line #93')
  vm_demFeSector[is.na(vm_demFeSector)] <- 0
  # FE non-energy use
message('line #96')
  vm_demFENonEnergySector <- readGDX(gdx, "vm_demFENonEnergySector", field = "l", restore_zeros = T, react = "silent")[,t,]*TWa_2_EJ
message('line #98')
  if (length(vm_demFENonEnergySector) == 0) {
message('line #100')
    vm_demFENonEnergySector <- NULL
  }

  # only retain combinations of SE, FE, sector, and emiMkt which actually exist in the model (see qm_balFe)
message('line #105')
  vm_demFeSector <- vm_demFeSector[demFemapping]

  #adding transport gas empty object to keep support to transport complex module
message('line #109')
  if(all(grep("fegat", getItems(vm_demFeSector,3)) == 0)){
message('line #111')
    extended_vm_demFeSector <- new.magpie(getItems(vm_demFeSector,1),getItems(vm_demFeSector,2),c(getItems(vm_demFeSector,3),"segabio.fegat.trans.ETS","segafos.fegat.trans.ETS","segasyn.fegat.trans.ETS","segabio.fegat.trans.ES","segafos.fegat.trans.ES","segasyn.fegat.trans.ES","segabio.fegat.trans.other","segafos.fegat.trans.other","segasyn.fegat.trans.other"),fill=0, sets=getSets(vm_demFeSector))
message('line #113')
    extended_vm_demFeSector[,,c(getItems(vm_demFeSector,3))] <- vm_demFeSector[,,c(getItems(vm_demFeSector,3))]
message('line #115')
    vm_demFeSector <- extended_vm_demFeSector
message('line #117')
  }

  # only retain combinations of SE, FE, te which actually exist in the model (qm_balFe)
message('line #121')
  vm_prodFe <- vm_prodFe[se2fe]

  # FE demand per industry subsector
message('line #125')
  o37_demFeIndSub <- readGDX(gdx, "o37_demFeIndSub", restore_zeros = FALSE,
                             format = "first_found", react = 'silent')
message('line #128')
  if (!(is.null(o37_demFeIndSub) | 0 == length(o37_demFeIndSub))) {
message('line #130')
    o37_demFeIndSub <- o37_demFeIndSub[,t,]
message('line #132')
    o37_demFeIndSub[is.na(o37_demFeIndSub)] <- 0
    # convert to EJ
message('line #135')
    o37_demFeIndSub <- o37_demFeIndSub * TWa_2_EJ
message('line #137')
  }




  # temporary backwards compatability: this can be removed, once a new test gdx after March 2021 is used
message('line #144')
  if ("seliqsyn" %in% getNames(vm_prodFe, dim=1)) {
message('line #146')
    seliq <- c("seliqfos","seliqbio","seliqsyn")
message('line #148')
    segas <- c("segafos","segabio","segasyn")
message('line #150')
  } else {
message('line #152')
    seliq <- c("seliqfos","seliqbio")
message('line #154')
    segas <- c("segafos","segabio")
message('line #156')
  }
message('line #158')
  ##


  ####### Realisation specific Variables ##########

  # Define current realisation for the different modules
  module2realisation <- readGDX(gdx, "module2realisation")
message('line #166')
  rownames(module2realisation) <- module2realisation$modules

message('line #169')
  find_real_module <- function(module_set, module_name){
message('line #171')
    return(module_set[module_set$modules == module_name,2])
  }

message('line #175')
  tran_mod = find_real_module(module2realisation,"transport")
message('line #177')
  indu_mod = find_real_module(module2realisation,"industry")
message('line #179')
  buil_mod = find_real_module(module2realisation,"buildings")
message('line #181')
  cdr_mod  = find_real_module(module2realisation,"CDR")

message('line #184')
  any_process_based <- length(readGDX(gdx, "secInd37Prc", react='silent')) > 0.
message('line #186')
  steel_process_based <- "steel" %in% readGDX(gdx, "secInd37Prc", react='silent')



  # Preliminary Calculations ----


  # calculate FE non-energy use and FE without non-energy use
message('line #195')
  if (!is.null(vm_demFENonEnergySector)) {
message('line #197')
    vm_demFENonEnergySector <-  mselect(vm_demFENonEnergySector[demFemapping],
                                        all_enty1 = entyFe2sector2emiMkt_NonEn$all_enty,
                                        emi_sectors = entyFe2sector2emiMkt_NonEn$emi_sectors,
                                        all_emiMkt = entyFe2sector2emiMkt_NonEn$all_emiMkt)

    # calculate FE without non-energy use
message('line #204')
    vm_demFeSector_woNonEn <- vm_demFeSector
message('line #206')
    vm_demFeSector_woNonEn[,,getNames(vm_demFENonEnergySector )] <- vm_demFeSector[,,getNames(vm_demFENonEnergySector )]-vm_demFENonEnergySector
message('line #208')
  }

  # ---- FE total production (incl. non-energy use) ------
message('line #212')
  out <- mbind(out,

    #total
message('line #216'),
    setNames((dimSums(vm_prodFe,dim=3,na.rm=T)), "FE (EJ/yr)"),

    #Liquids
message('line #220'),
    setNames(dimSums(vm_prodFe[,,seliq],dim=3,na.rm=T),                                                "FE|+|Liquids (EJ/yr)"),
message('line #222'),
    setNames(dimSums(vm_prodFe[,,"seliqbio"],dim=3,na.rm=T),                                                              "FE|Liquids|+|Biomass (EJ/yr)"),
message('line #224'),
    setNames(dimSums(vm_prodFe[,,"seliqfos"],dim=3,na.rm=T),                                                              "FE|Liquids|+|Fossil (EJ/yr)"),
message('line #226'),
    setNames(dimSums(mselect(vm_prodFe, all_enty="seliqsyn") ,dim=3,na.rm=T),                                             "FE|Liquids|+|Hydrogen (EJ/yr)"),

    #Solids
message('line #230'),
    setNames(dimSums(vm_prodFe[,,c("sesobio","sesofos")],dim=3,na.rm=T),                                                  "FE|+|Solids (EJ/yr)"),
message('line #232'),
    setNames(dimSums(vm_prodFe[,,"sesobio"],dim=3,na.rm=T),                                                               "FE|Solids|+|Biomass (EJ/yr)"),
message('line #234'),
    setNames(p_eta_conv[,,"tdbiosos"] * dimSums(mselect(vm_prodSe,all_enty1="sesobio",all_te="biotrmod"),dim=3,na.rm=T),  "FE|Solids|Biomass|+|Modern (EJ/yr)"),
message('line #236'),
    setNames(p_eta_conv[,,"tdbiosos"] * dimSums(mselect(vm_prodSe,all_enty1="sesobio",all_te="biotr"),dim=3,na.rm=T),     "FE|Solids|Biomass|+|Traditional (EJ/yr)"),
message('line #238'),
    setNames(dimSums(vm_prodFe[,,"sesofos"],dim=3,na.rm=T),                                                               "FE|Solids|+|Fossil (EJ/yr)"),
message('line #240'),
    setNames(p_eta_conv[,,"tdfossos"] * dimSums(mselect(vm_prodSe,all_enty1="sesofos",all_enty="pecoal"),dim=3,na.rm=T),  "FE|Solids|Fossil|+|Coal (EJ/yr)"),

    #Gases
message('line #244'),
    setNames(dimSums(vm_prodFe[,,segas],dim=3,na.rm=T),                                                  "FE|+|Gases (EJ/yr)"),
message('line #246'),
    setNames(dimSums(vm_prodFe[,,"segabio"],dim=3,na.rm=T),                                                               "FE|Gases|+|Biomass (EJ/yr)"),
message('line #248'),
    setNames(dimSums(vm_prodFe[,,"segafos"],dim=3,na.rm=T),                                                               "FE|Gases|+|Fossil (EJ/yr)"),
message('line #250'),
     setNames(dimSums(mselect(vm_prodFe, all_enty="segasyn") ,dim=3,na.rm=T),                                             "FE|Gases|+|Hydrogen (EJ/yr)"),


    # electricity
message('line #255'),
    setNames(dimSums(vm_prodFe[,,c("feels","feelt")],dim=3,na.rm=T),                                                      "FE|+|Electricity (EJ/yr)"),

    # heat
message('line #259'),
    setNames(dimSums(vm_prodFe[,,"sehe.fehes.tdhes"],dim=3,na.rm=T),                                                      "FE|+|Heat (EJ/yr)"),

    # hydrogen
message('line #263'),
    setNames(dimSums(vm_prodFe[,,c("feh2s","feh2t")],dim=3,na.rm=T),                                                      "FE|+|Hydrogen (EJ/yr)"),

    #Emission markets
message('line #267'),
    setNames((dimSums(mselect(vm_demFeSector, all_emiMkt="ES")  ,dim=3,na.rm=T)),                                         "FE|ESR (EJ/yr)"),
message('line #269'),
    setNames((dimSums(mselect(vm_demFeSector, all_emiMkt="ETS")  ,dim=3,na.rm=T)),                                        "FE|ETS (EJ/yr)"),
message('line #271'),
    setNames((dimSums(mselect(vm_demFeSector, all_emiMkt="other")  ,dim=3,na.rm=T)),                                      "FE|Outside ETS and ESR (EJ/yr)")
,message('line #273')
  )

message('line #276')
  getSets(out, fulldim=FALSE)[3] <- "data"

  # ---- FE sector demand ------



  #FE per sector and per emission market (ETS and ESR)
message('line #284')
  out <- mbind(out,

    #industry
message('line #288'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="indst")  ,dim=3,na.rm=T)),                                      "FE|++|Industry (EJ/yr)"),
message('line #290'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Industry|++|ESR (EJ/yr)"),
message('line #292'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|Industry|++|ETS (EJ/yr)"),

    # industry liquids
message('line #296'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|Industry|+|Liquids (EJ/yr)"),
message('line #298'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|Industry|Liquids|+|Biomass (EJ/yr)"),
message('line #300'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|Industry|Liquids|+|Fossil (EJ/yr)"),
message('line #302'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|Industry|Liquids|+|Hydrogen (EJ/yr)"),

message('line #305'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Industry|ESR|+|Liquids (EJ/yr)"),
message('line #307'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Liquids|+|Biomass (EJ/yr)"),
message('line #309'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Liquids|+|Fossil (EJ/yr)"),
message('line #311'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Liquids|+|Hydrogen (EJ/yr)"),

message('line #314'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|Industry|ETS|+|Liquids (EJ/yr)"),
message('line #316'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Liquids|+|Biomass (EJ/yr)"),
message('line #318'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Liquids|+|Fossil (EJ/yr)"),
message('line #320'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Liquids|+|Hydrogen (EJ/yr)"),

    # industry solids
message('line #324'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|Industry|+|Solids (EJ/yr)"),

message('line #327'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|Industry|Solids|+|Biomass (EJ/yr)"),
message('line #329'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|Industry|Solids|+|Fossil (EJ/yr)"),

message('line #332'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Industry|ESR|+|Solids (EJ/yr)"),
message('line #334'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Solids|+|Biomass (EJ/yr)"),
message('line #336'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Solids|+|Fossil (EJ/yr)"),

message('line #339'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|Industry|ETS|+|Solids (EJ/yr)"),
message('line #341'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Solids|+|Biomass (EJ/yr)"),
message('line #343'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Solids|+|Fossil (EJ/yr)"),

    # industry gases
message('line #347'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|Industry|+|Gases (EJ/yr)"),
message('line #349'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="indst"),dim=3,na.rm=T)),  "FE|Industry|Gases|+|Biomass (EJ/yr)"),
message('line #351'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|Industry|Gases|+|Fossil (EJ/yr)"),
message('line #353'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|Industry|Gases|+|Hydrogen (EJ/yr)"),

message('line #356'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Industry|ESR|+|Gases (EJ/yr)"),
message('line #358'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Gases|+|Biomass (EJ/yr)"),
message('line #360'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Gases|+|Fossil (EJ/yr)"),
message('line #362'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Gases|+|Hydrogen (EJ/yr)"),

message('line #365'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                  "FE|Industry|ETS|+|Gases (EJ/yr)"),
message('line #367'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Gases|+|Biomass (EJ/yr)"),
message('line #369'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Gases|+|Fossil (EJ/yr)"),
message('line #371'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Gases|+|Hydrogen (EJ/yr)"),

    # industry electricity
message('line #375'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="indst")  ,dim=3,na.rm=T)),                   "FE|Industry|+|Electricity (EJ/yr)"),
message('line #377'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Industry|ESR|+|Electricity (EJ/yr)"),
message('line #379'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Industry|ETS|+|Electricity (EJ/yr)"),

    # industry heat
message('line #383'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="indst")  ,dim=3,na.rm=T)),                   "FE|Industry|+|Heat (EJ/yr)"),
message('line #385'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Industry|ESR|+|Heat (EJ/yr)"),
message('line #387'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Industry|ETS|+|Heat (EJ/yr)"),

    # industry hydrogen
message('line #391'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="indst")  ,dim=3,na.rm=T)),                   "FE|Industry|+|Hydrogen (EJ/yr)"),
message('line #393'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Industry|ESR|+|Hydrogen (EJ/yr)"),
message('line #395'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Industry|ETS|+|Hydrogen (EJ/yr)"),


    # transport
    ## all transport variables include bunkers, except when expressly written otherwise in the variable name
message('line #401'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|++|Transport (EJ/yr)"),
message('line #403'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),    "FE|Transport|++|ESR (EJ/yr)"),
message('line #405'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)), "FE|Transport|++|Outside ETS and ESR (EJ/yr)"),

    # transport liquids
message('line #409'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|+|Liquids (EJ/yr)"),
message('line #411'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|Liquids|+|Biomass (EJ/yr)"),
message('line #413'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|Liquids|+|Fossil (EJ/yr)"),
message('line #415'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|Liquids|+|Hydrogen (EJ/yr)"),

message('line #418'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|ESR|+|Liquids (EJ/yr)"),
message('line #420'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Liquids|+|Biomass (EJ/yr)"),
message('line #422'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Liquids|+|Fossil (EJ/yr)"),
message('line #424'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Liquids|+|Hydrogen (EJ/yr)"),


message('line #428'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)),                    "FE|Transport|Outside ETS and ESR|+|Liquids (EJ/yr)"),
message('line #430'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Liquids|+|Biomass (EJ/yr)"),
message('line #432'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Liquids|+|Fossil (EJ/yr)"),
message('line #434'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Liquids|+|Hydrogen (EJ/yr)"),

message('line #437'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|LDV|Liquids|+|Biomass (EJ/yr)"),
message('line #439'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|LDV|Liquids|+|Fossil (EJ/yr)"),
message('line #441'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|LDV|Liquids|+|Hydrogen (EJ/yr)"),

message('line #444'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Liquids|+|Biomass (EJ/yr)"),
message('line #446'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Liquids|+|Fossil (EJ/yr)"),
message('line #448'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Liquids|+|Hydrogen (EJ/yr)"),

    # transport gases
message('line #452'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans")  ,dim=3,na.rm=T)),                    "FE|Transport|+|Gases (EJ/yr)"),
message('line #454'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans"),dim=3,na.rm=T)),  "FE|Transport|Gases|+|Biomass (EJ/yr)"),
message('line #456'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans") ,dim=3,na.rm=T)),  "FE|Transport|Gases|+|Fossil (EJ/yr)"),
message('line #458'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans") ,dim=3,na.rm=T)),  "FE|Transport|Gases|+|Hydrogen (EJ/yr)"),

message('line #461'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Transport|ESR|+|Gases (EJ/yr)"),
message('line #463'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Gases|+|Biomass (EJ/yr)"),
message('line #465'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Gases|+|Fossil (EJ/yr)"),
message('line #467'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Gases|+|Hydrogen (EJ/yr)"),

message('line #470'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)),                   "FE|Transport|Outside ETS and ESR|+|Gases (EJ/yr)"),
message('line #472'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Gases|+|Biomass (EJ/yr)"),
message('line #474'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Gases|+|Fossil (EJ/yr)"),
message('line #476'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Gases|+|Hydrogen (EJ/yr)"),

    # transport electricity
message('line #480'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|+|Electricity (EJ/yr)"),
message('line #482'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),    "FE|Transport|ESR|+|Electricity (EJ/yr)"),
message('line #484'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|+|Electricity (EJ/yr)"),

    # transport hydrogen
message('line #488'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|+|Hydrogen (EJ/yr)"),
message('line #490'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),    "FE|Transport|ESR|+|Hydrogen (EJ/yr)"),
message('line #492'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|+|Hydrogen (EJ/yr)"),


    # Buildings
message('line #497'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|++|Buildings (EJ/yr)"),
message('line #499'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|++|ESR (EJ/yr)"),
message('line #501'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|++|ETS (EJ/yr)"),

    # buildings liquids
message('line #505'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="build")  ,dim=3,na.rm=T)),                     "FE|Buildings|+|Liquids (EJ/yr)"),
message('line #507'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|+|Biomass (EJ/yr)"),
message('line #509'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|+|Fossil (EJ/yr)"),
message('line #511'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|+|Hydrogen (EJ/yr)"),

message('line #514'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Buildings|ESR|+|Liquids (EJ/yr)"),
message('line #516'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Liquids|+|Biomass (EJ/yr)"),
message('line #518'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Liquids|+|Fossil (EJ/yr)"),
message('line #520'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Liquids|+|Hydrogen (EJ/yr)"),


message('line #524'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|Buildings|ETS|+|Liquids (EJ/yr)"),
message('line #526'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Liquids|+|Biomass (EJ/yr)"),
message('line #528'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Liquids|+|Fossil (EJ/yr)"),
message('line #530'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Liquids|+|Hydrogen (EJ/yr)"),

    # buildings solids
message('line #534'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="build")  ,dim=3,na.rm=T)),                    "FE|Buildings|+|Solids (EJ/yr)"),

message('line #537'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Solids|+|Biomass (EJ/yr)"),
message('line #539'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Solids|+|Fossil (EJ/yr)"),

message('line #542'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Buildings|ESR|+|Solids (EJ/yr)"),
message('line #544'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Solids|+|Biomass (EJ/yr)"),
message('line #546'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Solids|+|Fossil (EJ/yr)"),

message('line #549'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|Buildings|ETS|+|Solids (EJ/yr)"),
message('line #551'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Solids|+|Biomass (EJ/yr)"),
message('line #553'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Solids|+|Fossil (EJ/yr)"),

    # buildings gases
message('line #557'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="build")  ,dim=3,na.rm=T)),                    "FE|Buildings|+|Gases (EJ/yr)"),
message('line #559'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="build"),dim=3,na.rm=T)),  "FE|Buildings|Gases|+|Biomass (EJ/yr)"),
message('line #561'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="build") ,dim=3,na.rm=T)),  "FE|Buildings|Gases|+|Fossil (EJ/yr)"),
message('line #563'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="build") ,dim=3,na.rm=T)),  "FE|Buildings|Gases|+|Hydrogen (EJ/yr)"),

message('line #566'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Buildings|ESR|+|Gases (EJ/yr)"),
message('line #568'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Gases|+|Biomass (EJ/yr)"),
message('line #570'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Gases|+|Fossil (EJ/yr)"),
message('line #572'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Gases|+|Hydrogen (EJ/yr)"),

message('line #575'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Gases (EJ/yr)"),
message('line #577'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Gases|+|Biomass (EJ/yr)"),
message('line #579'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Gases|+|Fossil (EJ/yr)"),
message('line #581'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Gases|+|Hydrogen (EJ/yr)"),

    # buildings electricity
message('line #585'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|Buildings|+|Electricity (EJ/yr)"),
message('line #587'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|ESR|+|Electricity (EJ/yr)"),
message('line #589'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Electricity (EJ/yr)"),

    # buildings heat
message('line #593'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|Buildings|+|Heat (EJ/yr)"),
message('line #595'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|ESR|+|Heat (EJ/yr)"),
message('line #597'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Heat (EJ/yr)"),

    # buildings hydrogen
message('line #601'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|Buildings|+|Hydrogen (EJ/yr)"),
message('line #603'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|ESR|+|Hydrogen (EJ/yr)"),
message('line #605'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Hydrogen (EJ/yr)"),

    # CDR
message('line #609'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="CDR")  ,dim=3,na.rm=T)),                  "FE|++|CDR (EJ/yr)"),
message('line #611'),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|+++|ETS (EJ/yr)"),

    # CDR liquids
message('line #615'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",emi_sectors="CDR")  ,dim=3,na.rm=T)),                     "FE|CDR|+|Liquids (EJ/yr)"),
message('line #617'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqbio",emi_sectors="CDR")  ,dim=3,na.rm=T)), "FE|CDR|Liquids|+|Biomass (EJ/yr)"),
message('line #619'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqfos",emi_sectors="CDR")  ,dim=3,na.rm=T)), "FE|CDR|Liquids|+|Fossil (EJ/yr)"),
message('line #621'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqsyn",emi_sectors="CDR")  ,dim=3,na.rm=T)), "FE|CDR|Liquids|+|Hydrogen (EJ/yr)"),

message('line #624'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|CDR|ETS|+|Liquids (EJ/yr)"),
message('line #626'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqbio",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Liquids|+|Biomass (EJ/yr)"),
message('line #628'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqfos",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Liquids|+|Fossil (EJ/yr)"),
message('line #630'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqsyn",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Liquids|+|Hydrogen (EJ/yr)"),
    # CDR gases
message('line #633'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Gases (EJ/yr)"),
message('line #635'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="CDR"),dim=3,na.rm=T)), "FE|CDR|Gases|+|Biomass (EJ/yr)"),
message('line #637'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="CDR") ,dim=3,na.rm=T)), "FE|CDR|Gases|+|Fossil (EJ/yr)"),
message('line #639'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="CDR") ,dim=3,na.rm=T)), "FE|CDR|Gases|+|Hydrogen (EJ/yr)"),

message('line #642'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|CDR|ETS|+|Gases (EJ/yr)"),
message('line #644'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Gases|+|Biomass (EJ/yr)"),
message('line #646'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Gases|+|Fossil (EJ/yr)"),
message('line #648'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Gases|+|Hydrogen (EJ/yr)"),

    # CDR electricity
message('line #652'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Electricity (EJ/yr)"),
message('line #654'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|ETS|+|Electricity (EJ/yr)"),

    # CDR hydrogen
message('line #658'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Hydrogen (EJ/yr)"),
message('line #660'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|ETS|+|Hydrogen (EJ/yr)"),

    # CDR heat
message('line #664'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Heat (EJ/yr)"),
message('line #666'),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|ETS|+|Heat (EJ/yr)")
,message('line #668')

  )

  # TODO: align these variables in transport complex and edge_esm!
  # quick fix: to avoid duplicates of this variable with the version with a "+" below in case of transport complex: only calculate this in case of edge_esm being used:

message('line #675')
  if (tran_mod != "complex"){
message('line #677')

  out <- mbind(out,
message('line #680'),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|LDV|Liquids (EJ/yr)"),
message('line #682'),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|non-LDV|Liquids (EJ/yr)"))
message('line #684')


  }
message('line #688')




  ### FE carriers from specific PE origin

message('line #695')
  p_share_coal_liq <- dimSums(mselect(vm_prodSe, all_enty="pecoal", all_enty1="seliqfos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="seliqfos"), dim=3, na.rm = T)
message('line #697')
  p_share_oil_liq <- dimSums(mselect(vm_prodSe, all_enty="peoil", all_enty1="seliqfos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="seliqfos"), dim=3, na.rm = T)
message('line #699')
  p_share_gas_liq <- dimSums(mselect(vm_prodSe, all_enty="pegas", all_enty1="seliqfos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="seliqfos"), dim=3, na.rm = T)


message('line #703')
  p_share_coal_liq[is.na(p_share_coal_liq)] <- 0
message('line #705')
  p_share_oil_liq[is.na(p_share_oil_liq)] <- 0
message('line #707')
  p_share_gas_liq[is.na(p_share_gas_liq)] <- 0

message('line #710')
  p_share_ngas_gas <- dimSums(mselect(vm_prodSe, all_enty="pegas", all_enty1="segafos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="segafos"), dim=3, na.rm = T)
message('line #712')
  p_share_coal_gas <- dimSums(mselect(vm_prodSe, all_enty="pecoal", all_enty1="segafos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="segafos"), dim=3, na.rm = T)

message('line #715')
  p_share_ngas_gas[is.na(p_share_ngas_gas)] <- 0
message('line #717')
  p_share_coal_gas[is.na(p_share_coal_gas)] <- 0

  # origin of fossil liquids and gases
message('line #721')
  out <- mbind(out,

                # industry fossil liquids
message('line #725'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="indst")*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Industry|Liquids|Fossil|+|Oil (EJ/yr)"),
message('line #727'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="indst")*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Industry|Liquids|Fossil|+|Gas (EJ/yr)"),
message('line #729'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="indst")*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Industry|Liquids|Fossil|+|Coal (EJ/yr)"),

                # buildings fossil liquids
message('line #733'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="build")*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|Fossil|+|Oil (EJ/yr)"),
message('line #735'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="build")*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|Fossil|+|Gas (EJ/yr)"),
message('line #737'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="build")*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Buildings|Liquids|Fossil|+|Coal (EJ/yr)"),

                # transport fossil liquids
message('line #741'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fepet","fedie"),emi_sectors="trans")*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Transport|Liquids|Fossil|+|Oil (EJ/yr)"),
message('line #743'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fepet","fedie"),emi_sectors="trans")*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Transport|Liquids|Fossil|+|Gas (EJ/yr)"),
message('line #745'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fepet","fedie"),emi_sectors="trans")*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Transport|Liquids|Fossil|+|Coal (EJ/yr)"),

                # industry fossil gases
message('line #749'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="indst")*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Industry|Gases|Fossil|+|Natural Gas (EJ/yr)"),
message('line #751'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="indst")*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Industry|Gases|Fossil|+|Coal (EJ/yr)"),

                # buildings fossil gases
message('line #755'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="build")*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Buildings|Gases|Fossil|+|Natural Gas (EJ/yr)"),
message('line #757'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="build")*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Buildings|Gases|Fossil|+|Coal (EJ/yr)"),

                # transport fossil gases
message('line #761'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegat",emi_sectors="trans")*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Transport|Gases|Fossil|+|Natural Gas (EJ/yr)"),
message('line #763'),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegat",emi_sectors="trans")*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Transport|Gases|Fossil|+|Coal (EJ/yr)"),

               # total fossil liquids
message('line #767'),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fehos","fepet","fedie"))*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Liquids|Fossil|+|Oil (EJ/yr)"),
message('line #769'),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fehos","fepet","fedie"))*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Liquids|Fossil|+|Gas (EJ/yr)"),
message('line #771'),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fehos","fepet","fedie"))*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Liquids|Fossil|+|Coal (EJ/yr)")
,message('line #773')


               # # total fossil gases, TODO: does not add up, check another time
               # setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1=c("fegas","fegat"))*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Gases|Fossil|+|Natural Gas (EJ/yr)"),
               # setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1=c("fegas","fegat"))*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Gases|Fossil|+|Coal (EJ/yr)")

  )

  # ---- BUNKERS ----
  # creating additional variables with and without bunkers for the transport sector

  ## ESR variables correspond to transport without bunkers by definition
message('line #786')
  var_without_Bunkers <- mbind(
    lapply(
      getNames(out)[grep("FE\\|Transport\\|ESR", getNames(out))],
      function(x) {
        setNames(out[,,x],gsub("FE\\|Transport\\|ESR","FE\\|Transport\\|w/o Bunkers",x))
      }
    )
  )
  ## Other emission market variables correspond to transport with bunkers by definition
message('line #796')
  var_with_Bunkers <- mbind(
    lapply(
      getNames(out)[grep("FE\\|Transport\\|Outside ETS and ESR", getNames(out))],
      function(x) {
        setNames(out[,,x],gsub("FE\\|Transport\\|Outside ETS and ESR","FE\\|Transport\\|Bunkers",x))
      }
    )
  )
message('line #805')
  out <- mbind(out,var_without_Bunkers,var_with_Bunkers)
  ##
message('line #808')
  out <- mbind(out,
    setNames(out[,,"FE|Transport|++|ESR (EJ/yr)"], "FE|Transport|w/o Bunkers (EJ/yr)"),
    setNames(out[,,"FE|Transport|++|Outside ETS and ESR (EJ/yr)"], "FE|Transport|Bunkers (EJ/yr)")
  )
message('line #813')
 out <- mbind(out,
               setNames(out[,,"FE (EJ/yr)"] - out[,,"FE|Transport|Bunkers (EJ/yr)"], "FE|w/o Bunkers (EJ/yr)")
  )



  # ---- read in needed data

  # ---- sets

  # ---- parameter
message('line #825')
  pm_cesdata <- readGDX(gdx,"pm_cesdata")[,t,]

  # ---- variables
message('line #829')
  if((buil_mod %in% c("services_putty", "services_with_capital"))||(tran_mod == "edge_esm") ) {
message('line #831')
    vm_demFeForEs <- readGDX(gdx,name = c("vm_demFeForEs"), field="l", restore_zeros=FALSE,format= "first_found",react = "silent")[,t,]*TWa_2_EJ
  }
  #vm_demFeForEs = vm_demFeForEs[fe2es]
  # CES nodes, convert from TWa to EJ
message('line #836')
  vm_cesIO <- readGDX(gdx, name=c("vm_cesIO"), field="l", restore_zeros=FALSE,format= "first_found")[,t,]*TWa_2_EJ

message('line #839')
  if(any_process_based){
message('line #841')
    vm_outflowPrc <- readGDX(gdx, name=c("vm_outflowPrc"), field="l", restore_zeros=FALSE, format="first_found", react='silent')[,t,]
message('line #843')
    o37_demFePrc <- readGDX(gdx, name=c("o37_demFePrc"), restore_zeros=FALSE,format= "first_found")[,t,] * TWa_2_EJ
message('line #845')
    o37_demFePrc [is.na(o37_demFePrc )] = 0.
message('line #847')
    o37_ProdIndRoute <- readGDX(gdx, name=c("o37_ProdIndRoute"), restore_zeros=FALSE, format="first_found", react='silent')[,t,]
message('line #849')
    o37_ProdIndRoute[is.na(o37_ProdIndRoute)] = 0.
message('line #851')
    o37_demFeIndRoute <- readGDX(gdx, name=c("o37_demFeIndRoute"), restore_zeros=FALSE, format="first_found", react='silent')[,t,] * TWa_2_EJ
message('line #853')
    o37_demFeIndRoute[is.na(o37_demFeIndRoute)] = 0.
    # mapping of process to output materials
message('line #856')
    tePrc2ue <- readGDX(gdx, "tePrc2ue", restore_zeros=FALSE)
message('line #858')
  }

  # ---- transformations
  # Correct for offset quantities in the transition between ESM and CES for zero quantities
message('line #863')
  if (any(grep('\\.offset_quantity$', getNames(pm_cesdata)))) {
message('line #865')
    pf <- paste0(getNames(vm_cesIO), '.offset_quantity')
message('line #867')
    offset <- collapseNames(pm_cesdata[,,pf]) * TWa_2_EJ
message('line #869')
    vm_cesIO <- vm_cesIO + offset[,t,getNames(vm_cesIO)]
message('line #871')
  }


  # ---- Buildings Module ----

message('line #877')
  p36_floorspace <- readGDX(gdx, "p36_floorspace", react = "silent")[, t, ]
message('line #879')
  if (!is.null(p36_floorspace)) {
message('line #881')
    if (dim(p36_floorspace)[3] > 1) {
message('line #883')
      out <- mbind(out,
message('line #885'),
                  setNames(p36_floorspace[, , "buildings"],   "ES|Buildings|Floor Space (bn m2)"),
message('line #887'),
                  setNames(p36_floorspace[, , "residential"], "ES|Buildings|Residential|Floor Space (bn m2)"),
message('line #889'),
                  setNames(p36_floorspace[, , "commercial"],  "ES|Buildings|Commercial|Floor Space (bn m2)"))
message('line #891')
    } else {
message('line #893')
      out <- mbind(out, setNames(p36_floorspace, "ES|Buildings|Floor Space (bn m2)"))
message('line #895')
    }
message('line #897')
  }

message('line #900')
  if (buil_mod == "simple") {
    # PPF in REMIND and the respective reporting variables
message('line #903')
    carrierBuild <- c(
      feelcb  = "FE|Buildings|non-Heating|Electricity|Conventional (EJ/yr)",
      feelrhb = "FE|Buildings|Heating|Electricity|Resistance (EJ/yr)",
      feelhpb = "FE|Buildings|Heating|Electricity|Heat pump (EJ/yr)",
      feheb   = "FE|Buildings|Heating|District Heating (EJ/yr)",
      fesob   = "FE|Buildings|Heating|Solids (EJ/yr)",
      fehob   = "FE|Buildings|Heating|Liquids (EJ/yr)",
      fegab   = "FE|Buildings|Heating|Gases (EJ/yr)",
      feh2b   = "FE|Buildings|Heating|Hydrogen (EJ/yr)")
    # all final energy (FE) demand in buildings without coventional electricity
    # is summed as heating
message('line #915')
    carrierBuildHeating <- tail(carrierBuild, -1)

    # FE demand in buildings for each carrier
    # (electricity split: heat pumps, resistive heating, rest)
message('line #920')
    for (c in names(carrierBuild)) {
message('line #922')
      out <- mbind(out, setNames(dimSums(vm_cesIO[,, c], dim = 3, na.rm = TRUE),
                                 carrierBuild[c]))
    }

    # sum of heating FE demand
message('line #928')
    out <- mbind(out,
      setNames(dimSums(vm_cesIO[,, names(carrierBuildHeating)], dim = 3, na.rm = TRUE),
               "FE|Buildings|Heating (EJ/yr)"))

    # UE demand in buildings for each carrier
    # this buildings realisation only works on a FE level but the UE demand is
    # estimated here assuming the FE-UE efficiency of the basline (from EDGE-B)
message('line #936')
    p36_uedemand_build <- readGDX(gdx, "p36_uedemand_build", react = "silent")[, t, ]
message('line #938')
    if (!is.null(p36_uedemand_build)) {
message('line #940')
      pm_fedemand <- readGDX(gdx, "pm_fedemand")[, t, ]
message('line #942')
      feUeEff_build <- p36_uedemand_build[,, names(carrierBuild)] /
        pm_fedemand[,, names(carrierBuild)]
message('line #945')
      feUeEff_build[is.na(feUeEff_build) | is.infinite(feUeEff_build)] <- 1
      # assume efficiency for all gases also for H2
message('line #948')
      feUeEff_build[,, "feh2b"] <- setNames(feUeEff_build[,, "fegab"], "feh2b")
      # apply efficiency to get UE levels
message('line #951')
      uedemand_build <- vm_cesIO[,, names(carrierBuild)] * feUeEff_build
message('line #953')
      getItems(uedemand_build, 3) <-
        gsub("^FE", "UE", carrierBuild)[getItems(uedemand_build, 3)]
message('line #956')
      out <- mbind(out, uedemand_build)

      # sum of heating UE demand
message('line #960')
      out <- mbind(out,
        setNames(dimSums(out[,, gsub("^FE", "UE", carrierBuildHeating)],
                         dim = 3, na.rm = TRUE),
                 "UE|Buildings|Heating (EJ/yr)"))

      # sum of buildings UE demand
message('line #967')
      out <- mbind(out,
                   setNames(dimSums(out[,, gsub("^FE", "UE", carrierBuild)],
                                    dim = 3, na.rm = TRUE),
                            "UE|Buildings (EJ/yr)"))
message('line #972')
    }
  } else if (buil_mod %in% c("services_putty", "services_with_capital")){
message('line #975')

    # sets
    ppfen_build <- readGDX(gdx,c("ppfen_buildings_dyn36","ppfen_buildings_dyn28","ppfen_buildings"),format="first_found", react = "silent")
message('line #979')
    esty_build <-  readGDX(gdx,c("esty_dyn36"),format="first_found", react = "silent")

    #var
message('line #983')
    v_prodEs <- readGDX(gdx,name = c("vm_prodEs","v_prodEs"), field="l",restore_zeros = F, format = "first_found", react = "silent")[,t,]* TWa_2_EJ

message('line #986')
    ces_elec = c(grep("elb$", ppfen_build, value = T),grep("hpb$", ppfen_build, value = T))
message('line #988')
    es_elec = c(grep("elb$", esty_build, value = T),grep("hpb$", esty_build, value = T))
message('line #990')
    es_solids = c(grep("sob$", esty_build, value = T), grep("stb$", esty_build, value = T))
message('line #992')
    es_gas = grep("gab$", esty_build, value = T)
message('line #994')
    es_liq = grep("hob$", esty_build, value = T)
message('line #996')
    es_heat = grep("heb$", esty_build, value = T)
message('line #998')
    es_hydro = grep("h2b$", esty_build, value = T)

message('line #1001')
    putty_ue = c("uescb","ueshb","uealb","uecwb")

message('line #1004')
    out <- mbind(out,
                  # Useful Energy
message('line #1007'),
                  setNames(dimSums(vm_cesIO[,,"uealb"],dim=3,na.rm=T),        "UE|Buildings|Appliances and Light (EJ/yr)"),
message('line #1009'),
                  setNames(dimSums(vm_cesIO[,,"uecwb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water (EJ/yr)"),
message('line #1011'),
                  setNames(dimSums(vm_cesIO[,,"ueshb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating (EJ/yr)"),
message('line #1013'),
                  setNames(dimSums(vm_cesIO[,,"uescb"],dim=3,na.rm=T),        "UE|Buildings|Space Cooling (EJ/yr)"),

message('line #1016'),
                  setNames(dimSums(vm_cesIO[,,putty_ue],dim=3,na.rm=T),       "UE|Buildings (EJ/yr)"),

message('line #1019'),
                  setNames(dimSums(vm_cesIO[,,"uescb"],dim=3,na.rm=T),        "UE|Buildings|Space Cooling|Electricity (EJ/yr)"),

message('line #1022'),
                  setNames(dimSums(vm_cesIO[,,"uealb"],dim=3,na.rm=T),        "UE|Buildings|Appliances and Light|Electricity (EJ/yr)"),

message('line #1025'),
                  setNames(dimSums(v_prodEs[,,c("uecwsob","uecwstb")],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Solids (EJ/yr)"),
message('line #1027'),
                  setNames(dimSums(v_prodEs[,,"uecwsob"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Solids|Modern (EJ/yr)"),
message('line #1029'),
                  setNames(dimSums(v_prodEs[,,"uecwstb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Solids|Traditional (EJ/yr)"),
message('line #1031'),
                  setNames(dimSums(v_prodEs[,,"uecwelb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Electricity|Resistance (EJ/yr)"),
message('line #1033'),
                  setNames(dimSums(v_prodEs[,,"uecwheb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Heat (EJ/yr)"),
message('line #1035'),
                  setNames(dimSums(v_prodEs[,,"uecwgab"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Gases (EJ/yr)"),
message('line #1037'),
                  setNames(dimSums(v_prodEs[,,"uecwhob"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Liquids (EJ/yr)"),
message('line #1039'),
                  setNames(dimSums(v_prodEs[,,"uecwh2b"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Hydrogen (EJ/yr)"),
message('line #1041'),
                  setNames(dimSums(v_prodEs[,,"uecwhpb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Electricity|Heat pump (EJ/yr)"),
message('line #1043'),
                  setNames(dimSums(v_prodEs[,,c("uecwelb","uecwhpb")],dim=3,na.rm=T), "UE|Buildings|Cooking and Water|Electricity (EJ/yr)"),


message('line #1047'),
                  setNames(dimSums(v_prodEs[,,c("ueshsob","ueshstb")],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Solids (EJ/yr)"),
message('line #1049'),
                  setNames(dimSums(v_prodEs[,,"ueshsob"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Solids|Modern (EJ/yr)"),
message('line #1051'),
                  setNames(dimSums(v_prodEs[,,"ueshstb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Solids|Traditional (EJ/yr)"),
message('line #1053'),
                  setNames(dimSums(v_prodEs[,,"ueshelb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Electricity|Resistance (EJ/yr)"),
message('line #1055'),
                  setNames(dimSums(v_prodEs[,,"ueshheb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Heat (EJ/yr)"),
message('line #1057'),
                  setNames(dimSums(v_prodEs[,,"ueshgab"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Gases (EJ/yr)"),
message('line #1059'),
                  setNames(dimSums(v_prodEs[,,"ueshhob"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Liquids (EJ/yr)"),
message('line #1061'),
                  setNames(dimSums(v_prodEs[,,"ueshh2b"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Hydrogen (EJ/yr)"),
message('line #1063'),
                  setNames(dimSums(v_prodEs[,,"ueshhpb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Electricity|Heat pump (EJ/yr)"),
message('line #1065'),
                  setNames(dimSums(v_prodEs[,,c("ueshelb","ueshhpb")],dim=3,na.rm=T), "UE|Buildings|Space Heating|Electricity (EJ/yr)"),

                  # Final Energy
message('line #1069'),
                  setNames(dimSums(vm_cesIO[,,"fealelb"],dim=3,na.rm=T),        "FE|Buildings|Appliances and Light|Electricity (EJ/yr)"),

message('line #1072'),
                  setNames(dimSums(vm_demFeForEs[,,c("uecwsob","uecwstb")],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Solids (EJ/yr)"),
message('line #1074'),
                  setNames(dimSums(vm_demFeForEs[,,"uecwsob"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Solids|Modern (EJ/yr)"),
message('line #1076'),
                  setNames(dimSums(vm_demFeForEs[,,"uecwstb"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Solids|Traditional (EJ/yr)"),
message('line #1078'),
                  setNames(dimSums(vm_demFeForEs[,,"uecwelb"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Electricity|Resistance (EJ/yr)"),
message('line #1080'),
                  setNames(dimSums(vm_demFeForEs[,,"uecwheb"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Heat (EJ/yr)"),
message('line #1082'),
                  setNames(dimSums(vm_demFeForEs[,,"uecwgab"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Gases (EJ/yr)"),
message('line #1084'),
                  setNames(dimSums(vm_demFeForEs[,,"uecwhob"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Liquids (EJ/yr)"),
message('line #1086'),
                  setNames(dimSums(vm_demFeForEs[,,"uecwh2b"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Hydrogen (EJ/yr)"),
message('line #1088'),
                  setNames(dimSums(vm_demFeForEs[,,"uecwhpb"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Electricity|Heat pump (EJ/yr)"),

message('line #1091'),
                  setNames(dimSums(vm_demFeForEs[,,c("uecwelb","uecwhpb")],dim=3,na.rm=T), "FE|Buildings|Cooking and Water|Electricity (EJ/yr)"),

message('line #1094'),
                  setNames(dimSums(vm_cesIO[,,"fescelb"],dim=3,na.rm=T),        "FE|Buildings|Space Cooling|Electricity (EJ/yr)"),

message('line #1097'),
                  setNames(dimSums(vm_demFeForEs[,,c("ueshsob","ueshstb")],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Solids (EJ/yr)"),
message('line #1099'),
                  setNames(dimSums(vm_demFeForEs[,,"ueshsob"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Solids|Modern (EJ/yr)"),
message('line #1101'),
                  setNames(dimSums(vm_demFeForEs[,,"ueshstb"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Solids|Traditional (EJ/yr)"),
message('line #1103'),
                  setNames(dimSums(vm_demFeForEs[,,"ueshelb"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Electricity|Resistance (EJ/yr)"),
message('line #1105'),
                  setNames(dimSums(vm_demFeForEs[,,"ueshheb"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Heat (EJ/yr)"),
message('line #1107'),
                  setNames(dimSums(vm_demFeForEs[,,"ueshgab"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Gases (EJ/yr)"),
message('line #1109'),
                  setNames(dimSums(vm_demFeForEs[,,"ueshhob"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Liquids (EJ/yr)"),
message('line #1111'),
                  setNames(dimSums(vm_demFeForEs[,,"ueshh2b"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Hydrogen (EJ/yr)"),
message('line #1113'),
                  setNames(dimSums(vm_demFeForEs[,,"ueshhpb"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Electricity|Heat pump (EJ/yr)"),
message('line #1115'),
                  setNames(dimSums(vm_demFeForEs[,,c("ueshelb","ueshhpb")],dim=3,na.rm=T), "FE|Buildings|Space Heating|Electricity (EJ/yr)")#,
,message('line #1117')

    )
message('line #1120')

    out <- mbind(out,
message('line #1123'),
                  setNames(dimSums(out[,,c("UE|Buildings|Space Heating (EJ/yr)","UE|Buildings|Space Cooling (EJ/yr)")],dim=3,na.rm=T), "UE|Buildings|Space Conditioning (EJ/yr)"),

message('line #1126'),
                  setNames(dimSums(out[,,"FE|Buildings|Appliances and Light|Electricity (EJ/yr)"],dim=3,na.rm=T),        "FE|Buildings|Appliances and Light (EJ/yr)"),

message('line #1129'),
                  setNames(dimSums(out[,,c("FE|Buildings|Cooking and Water|Solids (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Electricity (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Heat (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Gases (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Liquids (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Hydrogen (EJ/yr)")],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water (EJ/yr)"),

message('line #1137'),
                  setNames(dimSums(out[,,"FE|Buildings|Space Cooling|Electricity (EJ/yr)"],dim=3,na.rm=T),        "FE|Buildings|Space Cooling (EJ/yr)"),

message('line #1140'),
                  setNames(dimSums(out[,,c("FE|Buildings|Space Heating|Solids (EJ/yr)",
                                            "FE|Buildings|Space Heating|Electricity (EJ/yr)",
                                            "FE|Buildings|Space Heating|Heat (EJ/yr)",
                                            "FE|Buildings|Space Heating|Gases (EJ/yr)",
                                            "FE|Buildings|Space Heating|Liquids (EJ/yr)",
                                            "FE|Buildings|Space Heating|Hydrogen (EJ/yr)")],dim=3,na.rm=T),        "FE|Buildings|Space Heating (EJ/yr)")

,message('line #1148')
    )
message('line #1150')
  }

  # Industry Module ----
  ## FE demand ----
message('line #1155')
  if (!(is.null(o37_demFeIndSub) | 0 == length(o37_demFeIndSub))) {
    # this reporting is only available for GDXs which have the reporting
    # parameter o37_demFeIndSub

    # Big ol' table of variables to report, along with indices into
    # o37_demFeIndSub to select the right sets.  Indices can be either literal
    # strings or character vector objects.  NULL indices are not included in the
    # mselect() call.
message('line #1164')
    mixer <- tribble(
      ~variable,                                                 ~all_enty,   ~all_enty1,   ~secInd37,
      "FE|Industry|+++|Cement (EJ/yr)",                          NULL,        NULL,         "cement",
      "FE|Industry|Cement|+|Solids (EJ/yr)",                     NULL,        "fesos",      "cement",
      "FE|Industry|Cement|Solids|+|Fossil (EJ/yr)",              entySEfos,   "fesos",      "cement",
      "FE|Industry|Cement|Solids|+|Biomass (EJ/yr)",             entySEbio,   "fesos",      "cement",
      "FE|Industry|Cement|+|Liquids (EJ/yr)",                    NULL,        "fehos",      "cement",
      "FE|Industry|Cement|Liquids|+|Fossil (EJ/yr)",             entySEfos,   "fehos",      "cement",
      "FE|Industry|Cement|Liquids|+|Biomass (EJ/yr)",            entySEbio,   "fehos",      "cement",
      "FE|Industry|Cement|Liquids|+|Hydrogen (EJ/yr)",           entySEsyn,   "fehos",      "cement",
      "FE|Industry|Cement|+|Gases (EJ/yr)",                      NULL,        "fegas",      "cement",
      "FE|Industry|Cement|Gases|+|Fossil (EJ/yr)",               entySEfos,   "fegas",      "cement",
      "FE|Industry|Cement|Gases|+|Biomass (EJ/yr)",              entySEbio,   "fegas",      "cement",
      "FE|Industry|Cement|Gases|+|Hydrogen (EJ/yr)",             entySEsyn,   "fegas",      "cement",
      "FE|Industry|Cement|+|Hydrogen (EJ/yr)",                   NULL,        "feh2s",      "cement",
      "FE|Industry|Cement|+|Electricity (EJ/yr)",                NULL,        "feels",      "cement",

      "FE|Industry|+++|Chemicals (EJ/yr)",                       NULL,        NULL,         "chemicals",
      "FE|Industry|Chemicals|+|Solids (EJ/yr)",                  NULL,        "fesos",      "chemicals",
      "FE|Industry|Chemicals|Solids|+|Fossil (EJ/yr)",           entySEfos,   "fesos",      "chemicals",
      "FE|Industry|Chemicals|Solids|+|Biomass (EJ/yr)",          entySEbio,   "fesos",      "chemicals",
      "FE|Industry|Chemicals|+|Liquids (EJ/yr)",                 NULL,        "fehos",      "chemicals",
      "FE|Industry|Chemicals|Liquids|+|Fossil (EJ/yr)",          entySEfos,   "fehos",      "chemicals",
      "FE|Industry|Chemicals|Liquids|+|Biomass (EJ/yr)",         entySEbio,   "fehos",      "chemicals",
      "FE|Industry|Chemicals|Liquids|+|Hydrogen (EJ/yr)",        entySEsyn,   "fehos",      "chemicals",
      "FE|Industry|Chemicals|+|Gases (EJ/yr)",                   NULL,        "fegas",      "chemicals",
      "FE|Industry|Chemicals|Gases|+|Fossil (EJ/yr)",            entySEfos,   "fegas",      "chemicals",
      "FE|Industry|Chemicals|Gases|+|Biomass (EJ/yr)",           entySEbio,   "fegas",      "chemicals",
      "FE|Industry|Chemicals|Gases|+|Hydrogen (EJ/yr)",          entySEsyn,   "fegas",      "chemicals",
      "FE|Industry|Chemicals|+|Hydrogen (EJ/yr)",                NULL,        "feh2s",      "chemicals",
      "FE|Industry|Chemicals|+|Heat (EJ/yr)",                    NULL,        "fehes",      "chemicals",
      "FE|Industry|Chemicals|+|Electricity (EJ/yr)",             NULL,        "feels",      "chemicals",

      "FE|Industry|Steel|+|Solids (EJ/yr)",                      NULL,        "fesos",      "steel",
      "FE|Industry|+++|Steel (EJ/yr)",                           NULL,        NULL,         "steel",
      "FE|Industry|Steel|Solids|+|Fossil (EJ/yr)",               entySEfos,   "fesos",      "steel",
      "FE|Industry|Steel|Solids|+|Biomass (EJ/yr)",              entySEbio,   "fesos",      "steel",
      "FE|Industry|Steel|+|Liquids (EJ/yr)",                     NULL,        "fehos",      "steel",
      "FE|Industry|Steel|Liquids|+|Fossil (EJ/yr)",              entySEfos,   "fehos",      "steel",
      "FE|Industry|Steel|Liquids|+|Biomass (EJ/yr)",             entySEbio,   "fehos",      "steel",
      "FE|Industry|Steel|Liquids|+|Hydrogen (EJ/yr)",            entySEsyn,   "fehos",      "steel",
      "FE|Industry|Steel|+|Gases (EJ/yr)",                       NULL,        "fegas",      "steel",
      "FE|Industry|Steel|Gases|+|Fossil (EJ/yr)",                entySEfos,   "fegas",      "steel",
      "FE|Industry|Steel|Gases|+|Biomass (EJ/yr)",               entySEbio,   "fegas",      "steel",
      "FE|Industry|Steel|Gases|+|Hydrogen (EJ/yr)",              entySEsyn,   "fegas",      "steel",
      "FE|Industry|Steel|+|Hydrogen (EJ/yr)",                    NULL,        "feh2s",      "steel",
      "FE|Industry|Steel|+|Electricity (EJ/yr)",                 NULL,        "feels",      "steel",

      "FE|Industry|+++|Other Industry (EJ/yr)",                  NULL,        NULL,         "otherInd",
      "FE|Industry|Other Industry|+|Solids (EJ/yr)",             NULL,        "fesos",      "otherInd",
      "FE|Industry|Other Industry|Solids|+|Fossil (EJ/yr)",      entySEfos,   "fesos",      "otherInd",
      "FE|Industry|Other Industry|Solids|+|Biomass (EJ/yr)",     entySEbio,   "fesos",      "otherInd",
      "FE|Industry|Other Industry|+|Liquids (EJ/yr)",            NULL,        "fehos",      "otherInd",
      "FE|Industry|Other Industry|Liquids|+|Fossil (EJ/yr)",     entySEfos,   "fehos",      "otherInd",
      "FE|Industry|Other Industry|Liquids|+|Biomass (EJ/yr)",    entySEbio,   "fehos",      "otherInd",
      "FE|Industry|Other Industry|Liquids|+|Hydrogen (EJ/yr)",   entySEsyn,   "fehos",      "otherInd",
      "FE|Industry|Other Industry|+|Gases (EJ/yr)",              NULL,        "fegas",      "otherInd",
      "FE|Industry|Other Industry|Gases|+|Fossil (EJ/yr)",       entySEfos,   "fegas",      "otherInd",
      "FE|Industry|Other Industry|Gases|+|Biomass (EJ/yr)",      entySEbio,   "fegas",      "otherInd",
      "FE|Industry|Other Industry|Gases|+|Hydrogen (EJ/yr)",     entySEsyn,   "fegas",      "otherInd",
      "FE|Industry|Other Industry|+|Hydrogen (EJ/yr)",           NULL,        "feh2s",      "otherInd",
      "FE|Industry|Other Industry|+|Heat (EJ/yr)",               NULL,        "fehes",      "otherInd",
      "FE|Industry|Other Industry|+|Electricity (EJ/yr)",        NULL,        "feels",      "otherInd")

    # Convert a mixer table into a list that can be passed to mselect() to
    # select specified dimensions from a magpie object
message('line #1231')
    .mixer_to_selector <- function(mixer) {
message('line #1233')
      selector <- list()
message('line #1235')
      for (i in seq_len(nrow(mixer))) {
message('line #1237')
        selector <- c(
          selector,

          list(mixer[i,] %>%
                 as.list() %>%
                 # exclude list entries that are NULL
                 Filter(f = function(x) { !is.null(x[[1]]) }) %>%
                 # coerce character vector elements one level up
                 lapply(unlist))
        )
message('line #1248')
      }

message('line #1251')
      return(selector)
    }

    # call mselect(), dimSums(), setNames(), and multiply by factor
message('line #1256')
    .select_sum_name_multiply <- function(object, selector, factor = 1) {
message('line #1258')
      lapply(selector, function(x) {  # for each element in <selector>
        # call mselect() on <object>, but without the 'variable' element
        ( mselect(object, x[setdiff(names(x), 'variable')]) %>%
            dimSums(dim = 3) %>%
            setNames(x[['variable']])
        * factor
        )
      })
message('line #1267')
    }

    # calculate and bind to out
message('line #1271')
    out <- mbind(
      c(list(out), # pass a list of magpie objects
        .select_sum_name_multiply(o37_demFeIndSub, .mixer_to_selector(mixer))
      ))

message('line #1277')
    # subsectors realisation specific
    if (indu_mod == 'subsectors') {
message('line #1280')

      if (steel_process_based) {
message('line #1283')

        # technologies and operation modes that belong to primary and secondary steel
        teOpmoSteelPrimary <- tePrc2ue %>%
          filter(.data$all_in == "ue_steel_primary")
message('line #1288')
        teSteelPrimary <- teOpmoSteelPrimary %>% pull('tePrc')
message('line #1290')
        opmoSteelPrimary <- teOpmoSteelPrimary %>% pull('opmoPrc')
message('line #1292')
        teOpmoSteelSecondary <- tePrc2ue %>%
          filter(.data$all_in == "ue_steel_secondary")
message('line #1295')
        teSteelSecondary <- teOpmoSteelSecondary %>% pull('tePrc')
message('line #1297')
        opmoSteelSecondary <- teOpmoSteelSecondary %>% pull('opmoPrc')

        # Electricity uses by primary/secondary steel
message('line #1301')
        mixer <- tribble(
          ~variable,                                               ~all_enty,        ~all_te,           ~opmoPrc,
          "FE|Industry|Steel|Primary|Electricity (EJ/yr)",         "feels",          teSteelPrimary,    opmoSteelPrimary,
          "FE|Industry|Steel|Secondary|Electricity (EJ/yr)",       "feels",          teSteelSecondary,  opmoSteelSecondary,
          "FE|Industry|Steel|++|Primary (EJ/yr)",                  NULL,             teSteelPrimary,    opmoSteelPrimary,
          "FE|Industry|Steel|++|Secondary (EJ/yr)",                NULL,             teSteelSecondary,  opmoSteelSecondary)

message('line #1309')
        out <- mbind(
          c(list(out), # pass a list of magpie objects
            .select_sum_name_multiply(o37_demFePrc, .mixer_to_selector(mixer))
          ))

        # Electricity use by route
message('line #1316')
        mixer <- tribble(
          ~variable,                                      ~all_enty,  ~all_te,  ~route,           ~secInd37,
          "FE|Industry|Steel|+++|BF-BOF (EJ/yr)",         NULL,       NULL,     "bfbof",          "steel",
          "FE|Industry|Steel|+++|BF-BOF-CCS (EJ/yr)",     NULL,       NULL,     "bfbof_ccs",      "steel",
          "FE|Industry|Steel|+++|DRI-NG-EAF (EJ/yr)",     NULL,       NULL,     "idreaf_ng",      "steel",
          "FE|Industry|Steel|+++|DRI-NG-EAF-CCS (EJ/yr)", NULL,       NULL,     "idreaf_ng_ccs",  "steel",
          "FE|Industry|Steel|+++|DRI-H2-EAF (EJ/yr)",     NULL,       NULL,     "idreaf_h2",      "steel",
          "FE|Industry|Steel|+++|SCRAP-EAF (EJ/yr)",      NULL,       NULL,     "seceaf",         "steel")

message('line #1326')
        out <- mbind(
          c(list(out), # pass a list of magpie objects
            .select_sum_name_multiply(o37_demFeIndRoute, .mixer_to_selector(mixer))
          ))
message('line #1331')

      } else {
message('line #1334')

        # mapping of industrial output to energy production factors in CES tree
        ces_eff_target_dyn37 <- readGDX(gdx, "ces_eff_target_dyn37")

        # energy production factors for primary and secondary steel
message('line #1340')
        en.ppfen.primary.steel <- ces_eff_target_dyn37 %>%
          filter(.data$all_in == "ue_steel_primary") %>%
          pull('all_in1')
message('line #1344')
        en.ppfen.sec.steel <- ces_eff_target_dyn37 %>%
          filter(.data$all_in == "ue_steel_secondary") %>%
          pull('all_in1')

message('line #1349')
        mixer <- tribble(
          ~variable,                                                                                     ~all_in,
          "FE|Industry|Steel|Primary|Electricity (EJ/yr)",                                               "feel_steel_primary",
          "FE|Industry|Steel|Secondary|Electricity (EJ/yr)",                                             "feel_steel_secondary",
          "FE|Industry|Steel|++|Primary (EJ/yr)",                                                        en.ppfen.primary.steel,
          "FE|Industry|Steel|++|Secondary (EJ/yr)",                                                      en.ppfen.sec.steel)

        # calculate and bind to out
message('line #1358')
        out <- mbind(
          c(list(out), # pass a list of magpie objects
            .select_sum_name_multiply(vm_cesIO, .mixer_to_selector(mixer))
          ))
message('line #1363')
      }

message('line #1366')
      mixer <- tribble(
        ~variable,                                                                                     ~all_in,
        "FE|Industry|Chemicals|Electricity|+|Mechanical work and low-temperature heat (EJ/yr)",        "feelwlth_chemicals",
        "FE|Industry|Chemicals|Electricity|+|High-temperature heat (EJ/yr)",                           "feelhth_chemicals",
        "FE|Industry|Other Industry|Electricity|+|Mechanical work and low-temperature heat (EJ/yr)",   "feelwlth_otherInd",
        "FE|Industry|Other Industry|Electricity|+|High-temperature heat (EJ/yr)",                      "feelhth_otherInd")

      # calculate and bind to out
message('line #1375')
      out <- mbind(
        c(list(out), # pass a list of magpie objects
          .select_sum_name_multiply(vm_cesIO, .mixer_to_selector(mixer))
        ))
message('line #1380')
    }

    ## Industry Production/Value Added ----
    # reporting of industry production and value added as given by CES nodes
    # (only available in industry subsectors)
message('line #1386')
    if (indu_mod == 'subsectors') {
message('line #1388')
      mixer <- tribble(
        ~variable,                                                    ~all_in,
        "Production|Industry|Cement (Mt/yr)",                         "ue_cement",
        "Production|Industry|Steel (Mt/yr)",                          c("ue_steel_primary", "ue_steel_secondary"),
        "Production|Industry|Steel|Primary (Mt/yr)",                  "ue_steel_primary",
        "Production|Industry|Steel|Secondary (Mt/yr)",                "ue_steel_secondary",
        "Value Added|Industry|Chemicals (billion US$2005/yr)",        "ue_chemicals",
        "Value Added|Industry|Other Industry (billion US$2005/yr)",   "ue_otherInd")

      # calculate and bind to out
message('line #1399')
      out <- mbind(
        c(list(out), # pass a list of magpie objects
          # as vm_cesIO was multiplied by TWa_2_EJ in the beginning of the
          # script, needs to be converted back to REMIND units here and then
          # scaled by 1e3 for obtaining Mt or billion US$2005
          .select_sum_name_multiply(vm_cesIO, .mixer_to_selector(mixer),
                                    factor=1e3 / TWa_2_EJ),
          # report CES node of total industry as internal variable (for model
          # diagnostics) to represent total industry activity
          list(setNames(mselect(vm_cesIO, all_in = "ue_industry"),
                        "Internal|Activity|Industry (arbitrary unit/yr)"))
        )
      )


      # reporting of process-based industry production per process-route
message('line #1416')
      if (steel_process_based) {
message('line #1418')
        mixer <- tribble(
          ~variable,                                            ~mat,          ~route,
          "Production|Industry|Steel|+|BF-BOF (Mt/yr)",         "prsteel",     "bfbof",
          "Production|Industry|Steel|+|BF-BOF-CCS (Mt/yr)",     "prsteel",     "bfbof_ccs",
          "Production|Industry|Steel|+|DRI-NG-EAF (Mt/yr)",     "prsteel",     "idreaf_ng",
          "Production|Industry|Steel|+|DRI-NG-EAF-CCS (Mt/yr)", "prsteel",     "idreaf_ng_ccs",
          "Production|Industry|Steel|+|DRI-H2-EAF (Mt/yr)",     "prsteel",     "idreaf_h2",
          "Production|Industry|Steel|+|SCRAP-EAF (Mt/yr)",      "sesteel",     "seceaf"
        )

message('line #1429')
        out <- mbind(c(list(out),
                     .select_sum_name_multiply(o37_ProdIndRoute, .mixer_to_selector(mixer), factor=1e3))) # factor 1e3 converts Gt/yr to Mt/yr
message('line #1432')
      }
message('line #1434')
    }
message('line #1436')
  }

  #--- Transport reporting ---

  #Realization specific
message('line #1442')
  if (tran_mod == "complex"){

    #fedie = HDV, fepet = LDV
    #FE|Transport|LDV
    #FE|Transport|non-LDV
    #  FE|Transport|non-LDV|Bunkers
    #  FE|Transport|non-LDV|w/o Bunkers

    #Freight   > non-LDV > non bunker (national goods transportation)
    #                    > bunker (international goods transportation)
    #Passenger > non-LDV > non bunker
    #                    > bunker (international aviation and passenger ships)
    #Passenger > LDV

message('line #1457')
    v35_demTransType <- readGDX(gdx,name=c("v35_demTransType"),field="l",restore_zeros=FALSE,format="first_found")

message('line #1460')
    if(!is.null(v35_demTransType)){
message('line #1462')
      v35_demTransType <- v35_demTransType[,t,]*TWa_2_EJ

message('line #1465')
      out <- mbind(out,
                   # Transport LDV
message('line #1468'),
                   setNames((dimSums(mselect(v35_demTransType,transType_35="LDV")  ,dim=3,na.rm=T)),                  "FE|Transport|LDV (EJ/yr)"),
message('line #1470'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fepet",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|+|Liquids (EJ/yr)"),
message('line #1472'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|+|Gases (EJ/yr)"),
message('line #1474'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|+|Electricity (EJ/yr)"),
message('line #1476'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|+|Hydrogen (EJ/yr)"),

message('line #1479'),
                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)),                  "FE|Transport|LDV|ESR (EJ/yr)"),
message('line #1481'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fepet",all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|ESR|+|Liquids (EJ/yr)"),
message('line #1483'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|ESR|+|Gases (EJ/yr)"),
message('line #1485'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|ESR|+|Electricity (EJ/yr)"),
message('line #1487'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|ESR|+|Hydrogen (EJ/yr)"),

                   # Transport nonLDV
message('line #1491'),
                   setNames((dimSums(mselect(v35_demTransType,transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV (EJ/yr)"),
message('line #1493'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|+|Liquids (EJ/yr)"),
message('line #1495'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|+|Gases (EJ/yr)"),
message('line #1497'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|+|Electricity (EJ/yr)"),
message('line #1499'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|+|Hydrogen (EJ/yr)"),

message('line #1502'),
                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|ESR (EJ/yr)"),
message('line #1504'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|ESR|+|Liquids (EJ/yr)"),
message('line #1506'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|ESR|+|Gases (EJ/yr)"),
message('line #1508'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|ESR|+|Electricity (EJ/yr)"),
message('line #1510'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|ESR|+|Hydrogen (EJ/yr)"),

message('line #1513'),
                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|Outside ETS and ESR (EJ/yr)"),
message('line #1515'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Outside ETS and ESR|+|Liquids (EJ/yr)"),
message('line #1517'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Outside ETS and ESR|+|Gases (EJ/yr)"),
message('line #1519'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Outside ETS and ESR|+|Electricity (EJ/yr)"),
message('line #1521'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Outside ETS and ESR|+|Hydrogen (EJ/yr)"),

                   # Transport non-LDV Bunkers
message('line #1525'),
                   setNames((dimSums(mselect(v35_demTransType,transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|++|Bunkers (EJ/yr)"),
message('line #1527'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|+|Liquids (EJ/yr)"),
message('line #1529'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|+|Gases (EJ/yr)"),
message('line #1531'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|+|Electricity (EJ/yr)"),
message('line #1533'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|+|Hydrogen (EJ/yr)"),

message('line #1536'),
                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|Bunkers|++|Outside ETS and ESR (EJ/yr)"),
message('line #1538'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|Outside ETS and ESR|+|Liquids (EJ/yr)"),
message('line #1540'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|Outside ETS and ESR|+|Gases (EJ/yr)"),
message('line #1542'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|Outside ETS and ESR|+|Electricity (EJ/yr)"),
message('line #1544'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|Outside ETS and ESR|+|Hydrogen (EJ/yr)"),

                   # Transport non-LDV w/o Bunkers
message('line #1548'),
                   setNames((dimSums(mselect(v35_demTransType,transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|++|w/o Bunkers (EJ/yr)"),
message('line #1550'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|+|Liquids (EJ/yr)"),
message('line #1552'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|+|Gases (EJ/yr)"),
message('line #1554'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|+|Electricity (EJ/yr)"),
message('line #1556'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|+|Hydrogen (EJ/yr)"),

message('line #1559'),
                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|w/o Bunkers|++|ESR (EJ/yr)"),
message('line #1561'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|ESR|+|Liquids (EJ/yr)"),
message('line #1563'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|ESR|+|Gases (EJ/yr)"),
message('line #1565'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|ESR|+|Electricity (EJ/yr)"),
message('line #1567'),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|ESR|+|Hydrogen (EJ/yr)")
      )
message('line #1570')
    }

message('line #1573')
    p35_pass_FE_share_transp <- readGDX(gdx,"p35_pass_FE_share_transp", restore_zeros = FALSE)[,t,]

message('line #1576')
    v_demFe <- readGDX(gdx,name=c("v35_demFe","v_demFe"),field="l",restore_zeros=FALSE,format="first_found")[,t,]*TWa_2_EJ

message('line #1579')
    out <- mbind(out,

       # Passengers Liquids
message('line #1583'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),emi_sectors="trans")  ,dim=3,na.rm=T)) +
                (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp,                     "FE|Transport|Pass|+|Liquids (EJ/yr)"),
message('line #1586'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)) +
                (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Liquids|+|Biomass (EJ/yr)"),
message('line #1589'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)) +
                (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Liquids|+|Fossil (EJ/yr)"),
message('line #1592'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Liquids|+|Hydrogen (EJ/yr)"),

message('line #1596'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp,                     "FE|Transport|Pass|ESR|+|Liquids (EJ/yr)"),
message('line #1599'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|ESR|Liquids|+|Biomass (EJ/yr)"),
message('line #1602'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|ESR|Liquids|+|Fossil (EJ/yr)"),
message('line #1605'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|ESR|Liquids|+|Hydrogen (EJ/yr)"),

message('line #1609'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp,                     "FE|Transport|Pass|Outside ETS and ESR|+|Liquids (EJ/yr)"), #=bunkers
message('line #1611'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Outside ETS and ESR|Liquids|+|Biomass (EJ/yr)"),
message('line #1613'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Outside ETS and ESR|Liquids|+|Fossil (EJ/yr)"),
message('line #1615'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Outside ETS and ESR|Liquids|+|Hydrogen (EJ/yr)"),

message('line #1618'),
       setNames((dimSums(mselect(v_demFe,all_te="apCarPeT")  ,dim=3,na.rm=T)),                                                        "FE|Transport|Pass|Liquids|Road (EJ/yr)" ),

       # Freight Liquids
message('line #1622'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans") ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp),                      "FE|Transport|Freight|+|Liquids (EJ/yr)"),
message('line #1624'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Liquids|+|Biomass (EJ/yr)"),
message('line #1626'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Liquids|+|Fossil (EJ/yr)"),
message('line #1628'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Liquids|+|Hydrogn (EJ/yr)"),

message('line #1631'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp),                     "FE|Transport|Freight|ESR|+|Liquids (EJ/yr)"),
message('line #1633'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|ESR|Liquids|+|Biomass (EJ/yr)"),
message('line #1635'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|ESR|Liquids|+|Fossil (EJ/yr)"),
message('line #1637'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|ESR|Liquids|+|Hydrogen (EJ/yr)"),

message('line #1640'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp),                    "FE|Transport|Freight|Outside ETS and ESR|+|Liquids (EJ/yr)"),
message('line #1642'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Outside ETS and ESR|Liquids|+|Biomass (EJ/yr)"),
message('line #1644'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Outside ETS and ESR|Liquids|+|Fossil (EJ/yr)"),
message('line #1646'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Outside ETS and ESR|Liquids|+|Hydrogen (EJ/yr)"),

       # Passengers Gases
message('line #1650'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Transport|Pass|+|Gases (EJ/yr)"),
message('line #1652'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|Gases|+|Biomass (EJ/yr)"),
message('line #1654'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|Gases|+|Fossil (EJ/yr)"),
message('line #1656'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|Gases|+|Hydrogen (EJ/yr)"),

message('line #1659'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Transport|Pass|ESR|+|Gases (EJ/yr)"),
message('line #1661'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|ESR|Gases|+|Biomass (EJ/yr)"),
message('line #1663'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|ESR|Gases|+|Fossil (EJ/yr)"),
message('line #1665'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|ESR|Gases|+|Hydrogen (EJ/yr)"),

       # Passengers Electricity
message('line #1669'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|Pass|+|Electricity (EJ/yr)"),
message('line #1671'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|Pass|ESR|+|Electricity (EJ/yr)"),

message('line #1674'),
       setNames((dimSums(mselect(v_demFe,all_te="apTrnElT")  ,dim=3,na.rm=T)),                                                                 "FE|Transport|Pass|Electricity|Train (EJ/yr)" ),
message('line #1676'),
       setNames((dimSums(mselect(v_demFe,all_te="apCarElT")  ,dim=3,na.rm=T)),                                                                 "FE|Transport|Pass|Electricity|Road (EJ/yr)"),

       # Passengers hydrogen
message('line #1680'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|Pass|+|Hydrogen (EJ/yr)"),
message('line #1682'),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|Pass|ESR|+|Hydrogen (EJ/yr)")
,message('line #1684')
    )



    # transport technologies FE use
message('line #1690')
    out <- mbind(out,
message('line #1692'),
                 setNames((dimSums(mselect(v_demFe,all_te="apTrnElT")  ,dim=3,na.rm=T)),                          "FE|Transport|Pass|Train|Electricity (EJ/yr)" ),
message('line #1694'),
                 setNames((dimSums(mselect(v_demFe,all_te=c("apCarPeT","apCarH2T","apCarElT"))  ,dim=3,na.rm=T)), "FE|Transport|Pass|Road|LDV (EJ/yr)"),
message('line #1696'),
                 setNames((dimSums(mselect(v_demFe,all_te="apCarH2T")  ,dim=3,na.rm=T)),                          "FE|Transport|Pass|Road|LDV|Hydrogen (EJ/yr)"),
message('line #1698'),
                 setNames((dimSums(mselect(v_demFe,all_te="apCarPeT")  ,dim=3,na.rm=T)),                          "FE|Transport|Pass|Road|LDV|Liquids (EJ/yr)"),
message('line #1700'),
                 setNames((dimSums(mselect(v_demFe,all_te="apCarElT")  ,dim=3,na.rm=T)),                          "FE|Transport|Pass|Road|LDV|Electricity (EJ/yr)")
,message('line #1702')
    )

    # out <- mbind(out,
    #              setNames((dimSums(mselect(v_demFe,all_te=c("apCarDiT","apcarDiEffT","apcarDiEffH2T")),dim=3,na.rm=T)), "FE|Transport|non-LDV (EJ/yr)"),
    #              setNames((dimSums(mselect(v_demFe,all_te="apCarDiT")  ,dim=3,na.rm=T)),                                "FE|Transport|non-LDV|apCarDiT (EJ/yr)"),
    #              setNames((dimSums(mselect(v_demFe,all_te="apcarDiEffT")  ,dim=3,na.rm=T)),                             "FE|Transport|non-LDV|apcarDiEffT (EJ/yr)"),
    #              setNames((dimSums(mselect(v_demFe,all_te="apcarDiEffH2T")  ,dim=3,na.rm=T)),                           "FE|Transport|non-LDV|apcarDiEffH2T (EJ/yr)")
    #              )

message('line #1712')
    out <- mbind(out,
      # Passengers
message('line #1715'),
      setNames(out[,,"FE|Transport|Pass|+|Liquids (EJ/yr)"]       + out[,,"FE|Transport|Pass|+|Gases (EJ/yr)"]     + out[,,"FE|Transport|Pass|+|Electricity (EJ/yr)"]     + out[,,"FE|Transport|Pass|+|Hydrogen (EJ/yr)"]     , "FE|Transport|Pass (EJ/yr)"),
message('line #1717'),
      setNames(out[,,"FE|Transport|Pass|ESR|+|Liquids (EJ/yr)"]   + out[,,"FE|Transport|Pass|ESR|+|Gases (EJ/yr)"] + out[,,"FE|Transport|Pass|ESR|+|Electricity (EJ/yr)"] + out[,,"FE|Transport|Pass|ESR|+|Hydrogen (EJ/yr)"] , "FE|Transport|Pass|++|ESR (EJ/yr)"),
message('line #1719'),
      setNames(out[,,"FE|Transport|Pass|Outside ETS and ESR|+|Liquids (EJ/yr)"] , "FE|Transport|Pass|++|Outside ETS and ESR (EJ/yr)"),

      # Freight
message('line #1723'),
      setNames(out[,,"FE|Transport|Freight|+|Liquids (EJ/yr)"]       , "FE|Transport|Freight (EJ/yr)"),
message('line #1725'),
      setNames(out[,,"FE|Transport|Freight|ESR|+|Liquids (EJ/yr)"]   , "FE|Transport|Freight|++|ESR (EJ/yr)"),
message('line #1727'),
      setNames(out[,,"FE|Transport|Freight|Outside ETS and ESR|+|Liquids (EJ/yr)"] , "FE|Transport|Freight|++|Outside ETS and ESR (EJ/yr)")
,message('line #1729')
    )

    # BUNKERS

    ## ESR variables correspond to transport without bunkers by definition
message('line #1735')
    pass_var_without_Bunkers <- mbind(
      lapply(
        getNames(out)[grep("FE\\|Transport\\|Pass\\|ESR", getNames(out))],
        function(x) {
          setNames(out[,,x],gsub("FE\\|Transport\\|Pass\\|ESR","FE\\|Transport\\|Pass\\|w/o Bunkers",x))
        }
      )
    )
message('line #1744')
    freight_var_without_Bunkers <- mbind(
      lapply(
        getNames(out)[grep("FE\\|Transport\\|Freight\\|ESR", getNames(out))],
        function(x) {
          setNames(out[,,x],gsub("FE\\|Transport\\|Freight\\|ESR","FE\\|Transport\\|Freight\\|w/o Bunkers",x))
        }
      )
    )
    ## Outside ETS and ESR emission market variables correspond to transport with bunkers by definition
message('line #1754')
    pass_var_with_Bunkers <- mbind(
      lapply(
        getNames(out)[grep("FE\\|Transport\\|Pass\\|Outside ETS and ESR", getNames(out))],
        function(x) {
          setNames(out[,,x],gsub("FE\\|Transport\\|Pass\\|Outside ETS and ESR","FE\\|Transport\\|Pass\\|Bunkers",x))
        }
      )
    )
message('line #1763')
    freight_var_with_Bunkers <- mbind(
      lapply(
        getNames(out)[grep("FE\\|Transport\\|Freight\\|Outside ETS and ESR", getNames(out))],
        function(x) {
          setNames(out[,,x],gsub("FE\\|Transport\\|Freight\\|Outside ETS and ESR","FE\\|Transport\\|Freight\\|Bunkers",x))
        }
      )
    )
message('line #1772')
    out <- mbind(out,pass_var_without_Bunkers,freight_var_without_Bunkers,freight_var_with_Bunkers)

    ##
message('line #1776')
    out <- mbind(out,
message('line #1778'),
                 setNames(out[,,"FE|Transport|Pass|++|ESR (EJ/yr)"],   "FE|Transport|Pass|w/o Bunkers (EJ/yr)"),
message('line #1780'),
                 setNames(out[,,"FE|Transport|Pass|++|Outside ETS and ESR (EJ/yr)"], "FE|Transport|Pass|Bunkers (EJ/yr)"),

message('line #1783'),
                 setNames(out[,,"FE|Transport|Freight|++|ESR (EJ/yr)"],   "FE|Transport|Freight|w/o Bunkers (EJ/yr)"),
message('line #1785'),
                 setNames(out[,,"FE|Transport|Freight|++|Outside ETS and ESR (EJ/yr)"], "FE|Transport|Freight|Bunkers (EJ/yr)"),
message('line #1787')
    )

    # Energy Services
message('line #1791')
    p35_passLDV_ES_efficiency <- readGDX(gdx,"p35_passLDV_ES_efficiency", restore_zeros = FALSE)[,t,]
message('line #1793')
    p35_pass_nonLDV_ES_efficiency <- readGDX(gdx,"p35_pass_nonLDV_ES_efficiency", restore_zeros = FALSE)[,t,]
message('line #1795')
    p35_freight_ES_efficiency <- readGDX(gdx,"p35_freight_ES_efficiency", restore_zeros = FALSE)[,t,]

message('line #1798')
    out <- mbind(out,
message('line #1800'),
      setNames(dimSums(vm_cesIO[,,"ueLDVt"],dim=3,na.rm=T) * p35_passLDV_ES_efficiency,                                "ES|Transport|Pass|Road|LDV (bn pkm/yr)"),
message('line #1802'),
      setNames(p35_pass_nonLDV_ES_efficiency * p35_pass_FE_share_transp * dimSums(vm_cesIO[,,"ueHDVt"],dim=3,na.rm=T), "ES|Transport|Pass|non-LDV (bn pkm/yr)"),
message('line #1804')
    )

message('line #1807')
    out <- mbind(out,
message('line #1809'),
      setNames(p35_freight_ES_efficiency * (1-p35_pass_FE_share_transp) * dimSums(vm_cesIO[,,"ueHDVt"],dim=3,na.rm=T), "ES|Transport|Freight (bn tkm/yr)"),
message('line #1811'),
      setNames(out[,,"ES|Transport|Pass|Road|LDV (bn pkm/yr)"] + out[,,"ES|Transport|Pass|non-LDV (bn pkm/yr)"],       "ES|Transport|Pass (bn pkm/yr)")
,message('line #1813')
    )


    # ## Other variables (Kept temporarily for backwards compatibility) (need to double check these)
    # fe2ue <- readGDX(gdx,c("fe2ue", "fe2es"), format = "first_found")
    # LDV35 <- readGDX(gdx, "LDV35")
    #
    # v_demFe <- readGDX(gdx,name=c("v_demFe"),field="l",restore_zeros=FALSE,format="first_found")*TWa_2_EJ
    # v_demFe <- v_demFe[fe2ue]
    #
    # out <- mbind(out,
    #              setNames(dimSums(v_demFe[,,"apTrnElT"],dim=3), "FE|Transport|Pass|Train|Electricity (EJ/yr)" ),
    #              setNames(dimSums(v_demFe[,,LDV35],dim=3),      "FE|Transport|Pass|Road|LDV (EJ/yr)"),
    #              setNames(dimSums(v_demFe[,,"apCarH2T"],dim=3), "FE|Transport|Pass|Road|LDV|Hydrogen (EJ/yr)"),
    #              setNames(dimSums(v_demFe[,,"apCarPeT"],dim=3), "FE|Transport|Pass|Road|LDV|Liquids (EJ/yr)"),
    #              setNames(dimSums(v_demFe[,,"apCarElT"],dim=3), "FE|Transport|Pass|Road|LDV|Electricity (EJ/yr)")
    # )
    #
    # ## load conversion parameters
    # p35_passLDV_ES_efficiency <- readGDX(gdx,"p35_passLDV_ES_efficiency", restore_zeros = FALSE)
    # p35_pass_FE_share_transp <- readGDX(gdx,"p35_pass_FE_share_transp", restore_zeros = FALSE)
    # p35_freight_ES_efficiency <- readGDX(gdx,"p35_freight_ES_efficiency", restore_zeros = FALSE)
    # p35_pass_nonLDV_ES_efficiency <- readGDX(gdx,"p35_pass_nonLDV_ES_efficiency", restore_zeros = FALSE)
    #
    # #choose the CES entries names for transport
    # name_trsp=c("fepet","ueLDVt","fedie","ueHDVt","feelt","ueelTt","fepet_pass_sm","fedie_pass_sm","feelt_pass_sm","fedie_pass_lo","fedie_frgt_sm","feelt_frgt_sm","fedie_frgt_lo")
    # name_trsp=name_trsp[name_trsp%in%getNames(vm_cesIO)]
    #
    # name_trsp_HDV <- c("fedie","ueHDVt")
    # name_trsp_HDV=name_trsp_HDV[name_trsp_HDV%in%getNames(vm_cesIO)]
    # name_trsp_LDV <- c("fepet","ueLDVt")
    # name_trsp_LDV=name_trsp_LDV[name_trsp_LDV%in%getNames(vm_cesIO)]
    # name_trsp_ELT <- c("feelt","ueelTt")
    # name_trsp_ELT=name_trsp_ELT[name_trsp_ELT%in%getNames(vm_cesIO)]
    #
    # out <- mbind(out,
    #              setNames(out[,,"FE|Transport|w/o Bunkers (EJ/yr)"] - out[,,"FE|Transport|Pass|Road|LDV (EJ/yr)"], "FE|Transport|non-LDV (EJ/yr)"),
    #              setNames(dimSums(vm_cesIO[,,name_trsp_LDV],dim=3) * p35_passLDV_ES_efficiency, "ES|Transport|Pass|Road|LDV (bn pkm/yr)"),
    #              setNames(dimSums(vm_cesIO[,,name_trsp_LDV],dim=3), "UE|Transport|LDV (EJ/yr)"),
    #              setNames(dimSums(vm_cesIO[,,name_trsp_HDV],dim=3), "UE|Transport|HDV (EJ/yr)")
    # )
    #
    # out <- mbind(
    #   out,
    #   setNames(p35_pass_FE_share_transp * out[,, "FE|Transport|non-LDV (EJ/yr)"], "FE|Transport|Pass|non-LDV (EJ/yr)"),
    #   setNames(p35_pass_FE_share_transp * out[,, "UE|Transport|HDV (EJ/yr)"], "UE|Transport|Pass|non-LDV (EJ/yr)"),
    #   setNames((1- p35_pass_FE_share_transp) * out[,, "UE|Transport|HDV (EJ/yr)"], "UE|Transport|Freight (EJ/yr)")
    # )
    #
    # out <- mbind(out,
    #              setNames(p35_freight_ES_efficiency * out[,,"UE|Transport|Freight (EJ/yr)"],"ES|Transport|Freight (bn tkm/yr)"),
    #              setNames(p35_pass_nonLDV_ES_efficiency * out[,,"UE|Transport|Pass|non-LDV (EJ/yr)"],"ES|Transport|Pass|non-LDV (bn pkm/yr)")
    # )
    #
    # out <- mbind(out,
    #              setNames(out[,,"ES|Transport|Pass|Road|LDV (bn pkm/yr)"] + out[,,"ES|Transport|Pass|non-LDV (bn pkm/yr)"],"ES|Transport|Pass (bn pkm/yr)")
    # )

message('line #1872')
  }

message('line #1875')
  if (tran_mod == "edge_esm") {
    ## define the set that contains fe2es for transport
message('line #1878')
    fe2es_dyn35 <- readGDX(gdx,c("fe2es_dyn35"), format = "first_found")

message('line #1881')
    vm_demFeForEs_trnsp = vm_demFeForEs[fe2es_dyn35]

message('line #1884')
    out <- mbind(out,
message('line #1886'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Electricity (EJ/yr)"),
message('line #1888'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Electricity (EJ/yr)"),
message('line #1890'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Liquids (EJ/yr)"),
message('line #1892'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,c("esdie_pass_", "espet_pass_"),pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Liquids (EJ/yr)"),
message('line #1894'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Gases (EJ/yr)"),
message('line #1896'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Gases (EJ/yr)"),
message('line #1898'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Hydrogen (EJ/yr)"),
message('line #1900'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Hydrogen (EJ/yr)"),
message('line #1902'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Electricity (EJ/yr)"),
message('line #1904'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Electricity (EJ/yr)"),
message('line #1906'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Diesel Liquids (EJ/yr)"),
message('line #1908'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Diesel Liquids (EJ/yr)"),
message('line #1910'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"espet_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Petrol Liquids (EJ/yr)"),
message('line #1912'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_lo",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Long distance|Diesel Liquids (EJ/yr)"),
message('line #1914'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_pass_lo",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Long distance|Diesel Liquids (EJ/yr)"),
message('line #1916'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Gases (EJ/yr)"),
message('line #1918'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Gases (EJ/yr)"),
message('line #1920'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Hydrogen (EJ/yr)"),
message('line #1922'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Hydrogen (EJ/yr)"),
message('line #1924'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight (EJ/yr)"),
message('line #1926'),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass (EJ/yr)"),
message('line #1928'),
      setNames(dimSums(vm_cesIO[,,"entrp_frgt_",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # remove EJ conversion factor, conv. trillion to billion tkm
               "ES|Transport|Freight (bn tkm/yr)"),
message('line #1931'),
      setNames(dimSums(vm_cesIO[,,"entrp_pass_",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion pkm
               "ES|Transport|Pass (bn pkm/yr)"),
message('line #1934'),
      setNames(dimSums(vm_cesIO[,,"entrp_frgt_sm",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion tkm
               "ES|Transport|Freight|Short-Medium distance (bn tkm/yr)"),
message('line #1937'),
      setNames(dimSums(vm_cesIO[,,"entrp_pass_sm",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion pkm
               "ES|Transport|Pass|Short-Medium distance (bn pkm/yr)"),
message('line #1940'),
      setNames(dimSums(vm_cesIO[,,"entrp_frgt_lo",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion tkm
               "ES|Transport|Freight|Long distance (bn tkm/yr)"),
message('line #1943'),
      setNames(dimSums(vm_cesIO[,,"entrp_pass_lo",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion pkm
               "ES|Transport|Pass|Long distance (bn pkm/yr)"))


    # calculate total diesel and petrol liquids across all modes, needed in reportPrices
message('line #1949')
    out <- mbind(out,
message('line #1951'),
                 setNames(out[,,"FE|Transport|Pass|Short-Medium distance|Diesel Liquids (EJ/yr)"]+
                          out[,,"FE|Transport|Pass|Long distance|Diesel Liquids (EJ/yr)"] +
                          out[,,"FE|Transport|Freight|Short-Medium distance|Diesel Liquids (EJ/yr)"] +
                          out[,,"FE|Transport|Freight|Long distance|Diesel Liquids (EJ/yr)"],
                          "FE|Transport|Diesel Liquids (EJ/yr)"))

  }
message('line #1959')

  #--- CDR ---

  if(cdr_mod == "portfolio") {
message('line #1964')
    v33_FEdemand  <- readGDX(gdx, name=c("v33_FEdemand"), field="l", restore_zeros=F)[,t,] * TWa_2_EJ
    # KK: Mappings from gams set names to names in mifs. If new CDR methods are added to REMIND, please add
    # the method to CDR_te_list: "<method name in REMIND>"="<method name displayed in reporting>"
    # If a final energy carrier not included in CDR_FE_list is used, please also add it to the list.
message('line #1969')
    CDR_te_list <- list("dac"="DAC", "weathering"="EW")
message('line #1971')
    CDR_FE_list <- list("feels"="Electricity", "fegas"="Gases", "fehes"="Heat", "feh2s"="Hydrogen", "fedie"="Diesel")

    # loop to compute variables "FE|CDR|++|<CDR technology> (EJ/yr)" and "FE|CDR|<CDR technology>|+|<FE type> (EJ/yr)",
    # e.g., "FE|CDR|++|DAC (EJ/yr)" and "FE|CDR|DAC|+|Electricity (EJ/yr)"
message('line #1976')
    for (CDR_te in getItems(v33_FEdemand, dim="all_te")) {
message('line #1978')
      out <- mbind(out, setNames(dimSums(mselect(v33_FEdemand, all_te=CDR_te)),
                                 sprintf("FE|CDR|++|%s (EJ/yr)", CDR_te_list[[CDR_te]])))
      # loop over all FE technologies used by a given CDR technology CDR_te
message('line #1982')
      for (CDR_FE in getItems(mselect(v33_FEdemand, all_te=CDR_te), dim="all_enty")) {
message('line #1984')
        variable_name <- sprintf("FE|CDR|%s|+|%s (EJ/yr)", CDR_te_list[[CDR_te]], CDR_FE_list[[CDR_FE]])
message('line #1986')
        out <- mbind(out, setNames(dimSums(mselect(v33_FEdemand, all_te=CDR_te, all_enty=CDR_FE)),
                                   variable_name))
message('line #1989')
      }
message('line #1991')
    }
message('line #1993')
  }

message('line #1996')
  if(cdr_mod != "off" && cdr_mod != "portfolio"){ # compatibility with the CDR module before portfolio was added
message('line #1998')
    vm_otherFEdemand  <- readGDX(gdx,name=c("vm_otherFEdemand"),field="l",format="first_found")[,t,]*TWa_2_EJ

message('line #2001')
    s33_rockgrind_fedem <- readGDX(gdx,"s33_rockgrind_fedem", react = "silent")
message('line #2003')
    if (is.null(s33_rockgrind_fedem)){
message('line #2005')
      s33_rockgrind_fedem  <- new.magpie("GLO",NULL,fill=0)
message('line #2007')
    }
message('line #2009')
    v33_grindrock_onfield  <- readGDX(gdx,name=c("v33_grindrock_onfield"),field="l",format="first_found",react = "silent")[,t,]
message('line #2011')
    if (is.null(v33_grindrock_onfield)){
message('line #2013')
      v33_grindrock_onfield  <- new.magpie(getRegions(vm_otherFEdemand),getYears(vm_otherFEdemand),fill=0)
message('line #2015')
    }

message('line #2018')
    out <- mbind(out,
message('line #2020'),
                 setNames(vm_otherFEdemand[,,"feh2s"],        "FE|CDR|DAC|+|Hydrogen (EJ/yr)"),
message('line #2022'),
                 setNames(vm_otherFEdemand[,,"fegas"],        "FE|CDR|DAC|+|Gases (EJ/yr)"),
message('line #2024'),
                 setNames(vm_otherFEdemand[,,"fehes"],        "FE|CDR|DAC|+|Heat (EJ/yr)"),
message('line #2026'),
                 setNames(vm_otherFEdemand[,,"fedie"],        "FE|CDR|EW|+|Diesel (EJ/yr)"),
message('line #2028'),
                 setNames(s33_rockgrind_fedem*dimSums(v33_grindrock_onfield[,,],dim=3,na.rm=T),        "FE|CDR|EW|+|Electricity (EJ/yr)")
,message('line #2030')
    )
message('line #2032')
    out <- mbind(out,
message('line #2034'),
                 setNames(out[,,"FE|CDR|+|Electricity (EJ/yr)"] - out[,,"FE|CDR|EW|+|Electricity (EJ/yr)"], "FE|CDR|DAC|+|Electricity (EJ/yr)")
,message('line #2036')
    )
message('line #2038')
    out <- mbind(out,
message('line #2040'),
                 setNames(out[,,"FE|CDR|DAC|+|Hydrogen (EJ/yr)"] + out[,,"FE|CDR|DAC|+|Gases (EJ/yr)"] + out[,,"FE|CDR|DAC|+|Electricity (EJ/yr)"] + out[,,"FE|CDR|DAC|+|Heat (EJ/yr)"], "FE|CDR|++|DAC (EJ/yr)"),
message('line #2042'),
                 setNames(out[,,"FE|CDR|EW|+|Diesel (EJ/yr)"] + out[,,"FE|CDR|EW|+|Electricity (EJ/yr)"], "FE|CDR|++|EW (EJ/yr)")
,message('line #2044')
    )
message('line #2046')
 }

  #--- Additional Variables

message('line #2051')
  out <- mbind(out,
message('line #2053'),
    setNames(out[,,"FE (EJ/yr)"], "FE|Gross with CDR (EJ/yr)"),
message('line #2055'),
    setNames(out[,,"FE (EJ/yr)"] - out[,,"FE|++|CDR (EJ/yr)"], "FE|Net without CDR (EJ/yr)")
,message('line #2057')
  )

  # Add fuel computation
message('line #2061')
  out = mbind(out,
message('line #2063'),
    setNames(out[,,"FE|++|Buildings (EJ/yr)"] - out[,,"FE|Buildings|+|Electricity (EJ/yr)"] - out[,,"FE|Buildings|+|Heat (EJ/yr)"], "FE|Buildings|Fuels (EJ/yr)"),
message('line #2065'),
    setNames(out[,,"FE|++|Industry (EJ/yr)"]  - out[,,"FE|Industry|+|Electricity (EJ/yr)"]  - out[,,"FE|Industry|+|Heat (EJ/yr)"] , "FE|Industry|Fuels (EJ/yr)"),
message('line #2067'),
    setNames(out[,,"FE|++|Transport (EJ/yr)"] - out[,,"FE|Transport|+|Electricity (EJ/yr)"]                                       , "FE|Transport|Fuels (EJ/yr)"),
message('line #2069'),
    setNames(out[,,"FE (EJ/yr)"]             - out[,,"FE|+|Electricity (EJ/yr)"]           - out[,,"FE|+|Heat (EJ/yr)"]          , "FE|Fuels (EJ/yr)")
,message('line #2071')
  )



  # split sectoral biomass in modern and traditional for exogains
  # allocate tradional biomass to buildings first and only consider industry if
  # all biomass in buildings is traditional. All fossil solids are coal.
message('line #2079')
 out <- mbind(out, setNames(asS4(pmin(out[, , "FE|Solids|Biomass|+|Traditional (EJ/yr)"],
                                       out[, , "FE|Buildings|Solids|+|Biomass (EJ/yr)"])),
                             "FE|Buildings|Solids|Biomass|+|Traditional (EJ/yr)"))
message('line #2083')
  out <- mbind(out, setNames(out[, , "FE|Solids|Biomass|+|Traditional (EJ/yr)"] -
                               out[, , "FE|Buildings|Solids|Biomass|+|Traditional (EJ/yr)"] ,
                             "FE|Industry|Solids|Biomass|+|Traditional (EJ/yr)" ))
message('line #2087')
  out <- mbind(out,
message('line #2089'),
    setNames(out[, , "FE|Buildings|Solids|+|Biomass (EJ/yr)"] - out[, , "FE|Buildings|Solids|Biomass|+|Traditional (EJ/yr)"],
             "FE|Buildings|Solids|Biomass|+|Modern (EJ/yr)"),
message('line #2092'),
    setNames(out[, , "FE|Industry|Solids|+|Biomass (EJ/yr)"] - out[, , "FE|Industry|Solids|Biomass|+|Traditional (EJ/yr)"],
             "FE|Industry|Solids|Biomass|+|Modern (EJ/yr)"),
message('line #2095'),
    setNames(out[, , "FE|Buildings|Solids|+|Fossil (EJ/yr)"], "FE|Buildings|Solids|Coal (EJ/yr)"),
message('line #2097'),
    setNames(out[, , "FE|Industry|Solids|+|Fossil (EJ/yr)"], "FE|Industry|Solids|Coal (EJ/yr)")
,message('line #2099')
  )


  #### Non-energy Use Reporting ----

 ### temporary (!) industry non-energy use reporting
  # note: only for REMIND-EU SSP2

message('line #2108')
 if ("DEU" %in% getRegions(vm_prodFe) & indu_mod == 'subsectors' & is.null(vm_demFENonEnergySector)) {

      # read in FE industry non-energy use trajectories from industry subsectors run
message('line #2112')
      df.fe_nechem <- read.csv(system.file("extdata","pm_fe_nechem.cs4r",package = "remind2"),
                               sep = ",", skip = 4, header = F)
message('line #2115')
      colnames(df.fe_nechem) <- c("period", "region", "SSP", "encar","value_subsectors")

      # rescaling non-energy use to match 2020 EU27 values for total non-energy use
message('line #2119')
      EU27_regions <- c("DEU", "FRA", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN")

message('line #2122')
      df.fe_nechem <- df.fe_nechem %>%
        mutate(value_subsectors = ifelse(
          .data$region %in% EU27_regions,
          ( .data$value_subsectors
          * 3.835 # average between 2018-2021 = 3.835 EJ (https://ec.europa.eu/eurostat/databrowser/view/NRG_BAL_C__custom_6407922/bookmark/table?lang=en&bookmarkId=f7c8aa0e-3cf6-45d6-b85c-f2e76e90b4aa)
          / ( df.fe_nechem %>%
                filter(.data$region %in% EU27_regions,
                       .data$period == 2020,
                       .data$SSP == "SSP2") %>%
                summarize(value_subsectors = sum(.data$value_subsectors)) %>%
                pull(.data$value_subsectors)
            )
          ), # original 2020 df.fe_nechem total non-energy use
          .data$value_subsectors)
        )

      # non-energy use of solids/liquids/gases: min(fehoi,fehoi_nechem),
      # where fehoi would be the liquids of the current run and
      # fehoi_nechem the non-energy use liquids of the reference industry
      # subsectors run
message('line #2143')
      nechem_mixer <- tribble(
        ~data,                                    ~encar,
        "FE|Industry|+|Solids (EJ/yr)",             "fesoi_nechem",
        "FE|Industry|+|Liquids (EJ/yr)",            "fehoi_nechem",
        "FE|Industry|+|Gases (EJ/yr)",              "fegai_nechem")

message('line #2150')
      out.nechem <- out[,,nechem_mixer$data] %>%
        as_tibble() %>%
        inner_join(nechem_mixer, 'data') %>%
        inner_join(
          df.fe_nechem %>%
            filter(.data$period %in% getYears(out, TRUE),
                   .data$region %in% getItems(out, dim = 'all_regi'),
                   'SSP2' == .data$SSP),

          c('all_regi' = 'region', 'ttot' = 'period', 'encar')
        ) %>%
        mutate(Value_NonEn = pmin(.data$value, .data$value_subsectors),
               data = sub('^FE\\|Industry\\|', 'FE|Non-energy Use|Industry|',
                          .data$data)) %>%
        select(region = 'all_regi', period = 'ttot', encar = 'data',
               'Value_NonEn')

      # calculate to Non-energy FE by source (Fossil/Biomass/Hydrogen) as
      # Non-energy FE times share of source in total FE
message('line #2170')
      nechem_mixer_subvariables <- tribble(
        ~encar,                                            ~data,
        "FE|Non-energy Use|Industry|+|Solids (EJ/yr)",    "FE|Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)",
        "FE|Non-energy Use|Industry|+|Solids (EJ/yr)",    "FE|Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)",
        "FE|Non-energy Use|Industry|+|Liquids (EJ/yr)",   "FE|Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)",
        "FE|Non-energy Use|Industry|+|Liquids (EJ/yr)",   "FE|Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)",
        "FE|Non-energy Use|Industry|+|Liquids (EJ/yr)",   "FE|Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)",
        "FE|Non-energy Use|Industry|+|Gases (EJ/yr)",     "FE|Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)",
        "FE|Non-energy Use|Industry|+|Gases (EJ/yr)",     "FE|Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)",
        "FE|Non-energy Use|Industry|+|Gases (EJ/yr)",     "FE|Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)")

message('line #2182')
      out.nechem <- out.nechem %>%
        bind_rows(
          # get total Non-energy FE
          out.nechem %>%
            filter(.data$encar %in% nechem_mixer_subvariables$encar) %>%
            # combine with variable names by source
            full_join(nechem_mixer_subvariables, by = 'encar',
                      relationship = 'many-to-many') %>%
            # combine with FE data by source
            full_join(
              out %>%
                # filter `FE` data, not `FE|Non-Energy Use` data
                `[`(,,sub('\\|Non-energy Use', '',
                          nechem_mixer_subvariables$data)) %>%
                as_tibble() %>%
                # convert to `FE|Non-energy Use`, because we will use these
                # variable names
                mutate(data = sub('^FE\\|', 'FE|Non-energy Use|', .data$data)),

              by = c('region' = 'all_regi', 'period' = 'ttot', 'data')
            ) %>%
            group_by(.data$region, .data$period, .data$encar) %>%
            mutate(Value_NonEn = .data$Value_NonEn
                               * .data$value
                               / sum(.data$value)) %>%
            ungroup() %>%
            select('region', 'period', encar = 'data', 'Value_NonEn')
        ) %>%
        # fill mising data with zeros to please the magpie god
        complete(crossing(!!!syms(c('region', 'period', 'encar'))),
                 fill = list(Value_NonEn = 0)) %>%
        as.magpie(spatial = 1, temporal = 2, datacol = 4)

      # bind FE non-energy use to output object
message('line #2217')
      out <- mbind(out, out.nechem)

      # add further FE variables needed in ARIADNE
message('line #2221')
      out <- mbind(
        out,
message('line #2224'),
        setNames(  out[,,"FE|Non-energy Use|Industry|+|Solids (EJ/yr)"]
                 + out[,,"FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"]
                 + out[,,"FE|Non-energy Use|Industry|+|Gases (EJ/yr)"],
                 "FE|Non-energy Use|Industry (EJ/yr)")
      )

message('line #2231')
      out <- mbind(
        out,

message('line #2235'),
        setNames(  out[,,"FE (EJ/yr)"]
                 - out[,,"FE|Transport|Bunkers (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry (EJ/yr)"],
                 "FE|w/o Non-energy Use w/o Bunkers (EJ/yr)"),

message('line #2241'),
        setNames(  out[,,"FE|++|Industry (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry (EJ/yr)"),

message('line #2246'),
        setNames(  out[,,"FE|Industry|+|Solids (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|+|Solids (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Solids (EJ/yr)"),

message('line #2251'),
        setNames(  out[,,"FE|Industry|+|Liquids (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Liquids (EJ/yr)"),

message('line #2256'),
        setNames(  out[,,"FE|Industry|+|Gases (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|+|Gases (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Gases (EJ/yr)")
      )

message('line #2262')
      tryCatch(
        expr = {
message('line #2265')
          out <- mbind(
            out,

message('line #2269'),
            setNames(
                out[, , "FE|Industry|+++|Chemicals (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals (EJ/yr)"),

            # solids
message('line #2276'),
            setNames(
                out[, , "FE|Industry|Chemicals|+|Solids (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|+|Solids (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Solids (EJ/yr)"),

message('line #2282'),
            setNames(
                out[, , "FE|Industry|Chemicals|Solids|+|Fossil (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Solids|+|Fossil (EJ/yr)"),

message('line #2288'),
            setNames(
                out[, , "FE|Industry|Chemicals|Solids|+|Biomass (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Solids|+|Biomass (EJ/yr)"),

            # liquids
message('line #2295'),
            setNames(
                out[, , "FE|Industry|Chemicals|+|Liquids (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Liquids (EJ/yr)"),

message('line #2301'),
            setNames(
                out[, , "FE|Industry|Chemicals|Liquids|+|Fossil (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Liquids|+|Fossil (EJ/yr)"),

message('line #2307'),
            setNames(
                out[, , "FE|Industry|Chemicals|Liquids|+|Biomass (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Liquids|+|Biomass (EJ/yr)"),

message('line #2313'),
            setNames(
                out[, , "FE|Industry|Chemicals|Liquids|+|Hydrogen (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Liquids|+|Hydrogen (EJ/yr)"),

            # gases
message('line #2320'),
            setNames(
                out[, , "FE|Industry|Chemicals|+|Gases (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|+|Gases (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Gases (EJ/yr)"),

message('line #2326'),
            setNames(
                out[, , "FE|Industry|Chemicals|Gases|+|Fossil (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Gases|+|Fossil (EJ/yr)"),

message('line #2332'),
            setNames(
                out[, , "FE|Industry|Chemicals|Gases|+|Biomass (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Gases|+|Biomass (EJ/yr)"),

message('line #2338'),
            setNames(
                out[, , "FE|Industry|Chemicals|Gases|+|Hydrogen (EJ/yr)"]
              - out[, , "FE|Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Gases|+|Hydrogen (EJ/yr)")
,message('line #2343')
          )
        },
        error = function(e) {
          warning(e)
        }
      )

message('line #2351')
      out <- mbind(
        out,

        # solids
message('line #2356'),
        setNames(  out[,,"FE|Industry|Solids|+|Fossil (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"),

message('line #2361'),
        setNames(  out[,,"FE|Industry|Solids|+|Biomass (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"),

        # liquids
message('line #2367'),
        setNames(  out[,,"FE|Industry|Liquids|+|Fossil (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"),

message('line #2372'),
        setNames(  out[,,"FE|Industry|Liquids|+|Biomass (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"),

message('line #2377'),
        setNames(  out[,,"FE|Industry|Liquids|+|Hydrogen (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"),

        # gases
message('line #2383'),
        setNames(  out[,,"FE|Industry|Gases|+|Fossil (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"),

message('line #2388'),
        setNames(  out[,,"FE|Industry|Gases|+|Biomass (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"),

message('line #2393'),
        setNames(  out[,,"FE|Industry|Gases|+|Hydrogen (EJ/yr)"]
                 - out[,,"FE|Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)"],
                 "FE|w/o Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)")
,message('line #2397')
        )


      # total FE variables per energy carrier without bunkers and without non-energy use

 message('line #2403')
     out <- mbind(out,
                   # liquids
message('line #2406'),
                   setNames(out[,,"FE|+|Liquids (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|+|Liquids (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Liquids (EJ/yr)"),
                   # biomass liquids
message('line #2412'),
                   setNames(out[,,"FE|Liquids|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Liquids|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Liquids|Biomass (EJ/yr)"),
                   # fossil liquids
message('line #2418'),
                   setNames(out[,,"FE|Liquids|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Liquids|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Liquids|Fossil (EJ/yr)"),
                   # synthetic liquids
message('line #2424'),
                   setNames(out[,,"FE|Liquids|+|Hydrogen (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Liquids|+|Hydrogen (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Liquids|Hydrogen (EJ/yr)"),

                   # gases
message('line #2431'),
                   setNames(out[,,"FE|+|Gases (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|+|Gases (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|+|Gases (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Gases (EJ/yr)"),
                   # biomass Gases
message('line #2437'),
                   setNames(out[,,"FE|Gases|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Gases|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Gases|Biomass (EJ/yr)"),
                   # fossil Gases
message('line #2443'),
                   setNames(out[,,"FE|Gases|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Gases|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Gases|Fossil (EJ/yr)"),
                   # synthetic Gases
message('line #2449'),
                   setNames(out[,,"FE|Gases|+|Hydrogen (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Gases|+|Hydrogen (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Gases|Hydrogen (EJ/yr)"),

                   # solids
message('line #2456'),
                   setNames(out[,,"FE|+|Solids (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|+|Solids (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Solids (EJ/yr)"),
                   # biomass Solids
message('line #2461'),
                   setNames(out[,,"FE|Solids|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Solids|Biomass (EJ/yr)"),
                   # fossil Solids
message('line #2466'),
                   setNames(out[,,"FE|Solids|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Solids|Fossil (EJ/yr)"))
  }
  ### end of "### temporary (!) industry non-energy use reporting" for Ariadne

  # in case the current non-energy use implementation creates negative values, set them to 0
message('line #2474')
  if (any(out < 0)) {
    out[out < 0] <- 0
  }

# report feedstocks use by carrier when available
message('line #2480')
  if (!is.null(vm_demFENonEnergySector)) {
    # FE non-energy use variables
message('line #2483')
    out <- mbind(out,
message('line #2485'),
                  setNames(dimSums(vm_demFENonEnergySector, dim=3),
                           "FE|Non-energy Use (EJ/yr)"),

message('line #2489'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst"), dim=3),
                           "FE|Non-energy Use|+|Industry (EJ/yr)"),

message('line #2493'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fesos"), dim=3),
                           "FE|Non-energy Use|Industry|+|Solids (EJ/yr)"),
message('line #2496'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fehos"), dim=3),
                           "FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"),
message('line #2499'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fegas"), dim=3),
                          "FE|Non-energy Use|Industry|+|Gases (EJ/yr)")
,message('line #2502')
                  )


    # FE non-energy use per SE origin
message('line #2507')
   out <- mbind(out,
message('line #2509'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fesos", all_enty = "sesofos"), dim=3),
                           "FE|Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"),
message('line #2512'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fesos", all_enty = "sesobio"), dim=3),
                           "FE|Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"),
message('line #2515'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fehos", all_enty = "seliqfos"), dim=3),
                           "FE|Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"),
message('line #2518'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fehos", all_enty = "seliqbio"), dim=3),
                           "FE|Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"),
message('line #2521'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fehos", all_enty = "seliqsyn"), dim=3),
                           "FE|Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"),
message('line #2524'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fegas", all_enty = "segafos"), dim=3),
                           "FE|Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"),
message('line #2527'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fegas", all_enty = "segabio"), dim=3),
                           "FE|Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"),
message('line #2530'),
                  setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fegas", all_enty = "segasyn"), dim=3),
                           "FE|Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)")
,message('line #2533')
                  )

    ### FE without non-energy use
message('line #2537')
    out <- mbind(out,

                 #total
message('line #2541'),
                 setNames(dimSums(vm_demFeSector_woNonEn,dim=3),
                          "FE|w/o Non-energy Use (EJ/yr)"),

                 #Liquids
message('line #2546'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,c("fepet","fedie","fehos")],dim=3),                                                "FE|w/o Non-energy Use|Liquids (EJ/yr)"),
message('line #2548'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,"seliqbio"],dim=3),                                                       "FE|w/o Non-energy Use|Liquids|+|Biomass (EJ/yr)"),
message('line #2550'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,"seliqfos"],dim=3),                                                       "FE|w/o Non-energy Use|Liquids|+|Fossil (EJ/yr)"),
message('line #2552'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,"seliqsyn"] ,dim=3),                                                      "FE|w/o Non-energy Use|Liquids|+|Hydrogen (EJ/yr)"),

                 # Gases
message('line #2556'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,c("fegas","fegat")],dim=3),                                       "FE|w/o Non-energy Use|Gases (EJ/yr)"),
message('line #2558'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,"segabio"],dim=3),                                                       "FE|w/o Non-energy Use|Gases|+|Biomass (EJ/yr)"),
message('line #2560'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,"segafos"],dim=3),                                                       "FE|w/o Non-energy Use|Gases|+|Fossil (EJ/yr)"),
message('line #2562'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,"segasyn"] ,dim=3),                                                      "FE|w/o Non-energy Use|Gases|+|Hydrogen (EJ/yr)"),

                 # Solids
message('line #2566'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,"fesos"],dim=3),                                                         "FE|w/o Non-energy Use|Solids (EJ/yr)"),
message('line #2568'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,"sesobio"],dim=3),                                                       "FE|w/o Non-energy Use|Solids|+|Biomass (EJ/yr)"),
message('line #2570'),
                 setNames(dimSums(vm_demFeSector_woNonEn[,,"sesofos"],dim=3),                                                       "FE|w/o Non-energy Use|Solids|+|Fossil (EJ/yr)")
,message('line #2572')
    )

    #FE per sector and per emission market (ETS and ESR)
message('line #2576')
    out <- mbind(out,

                 #industry
message('line #2580'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,emi_sectors="indst")  ,dim=3,na.rm=T)),                                      "FE|w/o Non-energy Use|Industry (EJ/yr)"),
message('line #2582'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|w/o Non-energy Use|Industry|ESR (EJ/yr)"),
message('line #2584'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|ETS (EJ/yr)"),

                 # industry liquids
message('line #2588'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|Liquids (EJ/yr)"),
message('line #2590'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|w/o Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"),
message('line #2592'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|w/o Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"),
message('line #2594'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|w/o Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"),

message('line #2597'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|w/o Non-energy Use|Industry|ESR|Liquids (EJ/yr)"),
message('line #2599'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Liquids|+|Biomass (EJ/yr)"),
message('line #2601'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Liquids|+|Fossil (EJ/yr)"),
message('line #2603'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Liquids|+|Hydrogen (EJ/yr)"),

message('line #2606'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|ETS|Liquids (EJ/yr)"),
message('line #2608'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Liquids|+|Biomass (EJ/yr)"),
message('line #2610'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Liquids|+|Fossil (EJ/yr)"),
message('line #2612'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Liquids|+|Hydrogen (EJ/yr)"),

                 # industry solids
message('line #2616'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|Solids (EJ/yr)"),

message('line #2619'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"),
message('line #2621'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"),

message('line #2624'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|ESR|Solids (EJ/yr)"),
message('line #2626'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Solids|+|Biomass (EJ/yr)"),
message('line #2628'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Solids|+|Fossil (EJ/yr)"),

message('line #2631'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|w/o Non-energy Use|Industry|ETS|Solids (EJ/yr)"),
message('line #2633'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Solids|+|Biomass (EJ/yr)"),
message('line #2635'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Solids|+|Fossil (EJ/yr)"),

                 # industry gases
message('line #2639'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|Gases (EJ/yr)"),
message('line #2641'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segabio",emi_sectors="indst"),dim=3,na.rm=T)),  "FE|w/o Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"),
message('line #2643'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segafos",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|w/o Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"),
message('line #2645'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|w/o Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)"),

message('line #2648'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|ESR|Gases (EJ/yr)"),
message('line #2650'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Gases|+|Biomass (EJ/yr)"),
message('line #2652'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Gases|+|Fossil (EJ/yr)"),
message('line #2654'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Gases|+|Hydrogen (EJ/yr)"),

message('line #2657'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                  "FE|w/o Non-energy Use|Industry|ETS|Gases (EJ/yr)"),
message('line #2659'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Gases|+|Biomass (EJ/yr)"),
message('line #2661'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Gases|+|Fossil (EJ/yr)"),
message('line #2663'),
                 setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Gases|+|Hydrogen (EJ/yr)")
,message('line #2665')


    )
message('line #2669')

      tryCatch(
        expr = {
          out <- mbind(
            out,
message('line #2675'),
            setNames(
              out[, , "FE|Industry|Chemicals|+|Solids (EJ/yr)"] - out[, , "FE|Non-energy Use|Industry|+|Solids (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Solids (EJ/yr)"
            ),
message('line #2680'),
            setNames(
              out[, , "FE|Industry|Chemicals|+|Liquids (EJ/yr)"] - out[, , "FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Liquids (EJ/yr)"
            ),
message('line #2685'),
            setNames(
              out[, , "FE|Industry|Chemicals|+|Gases (EJ/yr)"] - out[, , "FE|Non-energy Use|Industry|+|Gases (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals|Gases (EJ/yr)"
            ),
message('line #2690'),
            setNames(
              out[, , "FE|Industry|+++|Chemicals (EJ/yr)"] - out[, , "FE|Non-energy Use|+|Industry (EJ/yr)"],
              "FE|w/o Non-energy Use|Industry|Chemicals (EJ/yr)"
            )
,message('line #2695')
          )
        },
        error = function(e) {
          warning(e)
        }
      )
  }
message('line #2703')
  ### FE variables without bunkers ----

  ### variables for which version without bunkers should be calculated

  fe.vars.woBunkers <- c(

    "FE (EJ/yr)",
    "FE|++|Transport (EJ/yr)",
    "FE|Transport|+|Liquids (EJ/yr)")

  # add FE w/o non-energy use variables if available
message('line #2715')
 if ("FE|Non-energy Use (EJ/yr)" %in% getNames(out)) {


message('line #2719')
    fe.vars.woBunkers <- c(fe.vars.woBunkers,
                          "FE|w/o Non-energy Use (EJ/yr)",
                          "FE|w/o Non-energy Use|Liquids (EJ/yr)")

  }

  # bunker correction for distinction of fossil, biomass, hydrogen-based liquids
message('line #2727')
  fe.vars.woBunkers.fos <- c(  "FE|Liquids|+|Fossil (EJ/yr)",
                               "FE|Transport|Liquids|+|Fossil (EJ/yr)")

message('line #2731')
  fe.vars.woBunkers.bio <- c(  "FE|Liquids|+|Biomass (EJ/yr)",
                               "FE|Transport|Liquids|+|Biomass (EJ/yr)")

message('line #2735')
  fe.vars.woBunkers.syn <- c(  "FE|Liquids|+|Hydrogen (EJ/yr)",
                               "FE|Transport|Liquids|+|Hydrogen (EJ/yr)")


  # add FE w/o non-energy use variables if available
message('line #2741')
  if ("FE|Non-energy Use (EJ/yr)" %in% getNames(out)) {

    # bunker correction for distinction of fossil, biomass, hydrogen-based liquids
message('line #2745')
    fe.vars.woBunkers.fos <- c(   fe.vars.woBunkers.fos,
                                 "FE|w/o Non-energy Use|Liquids|+|Fossil (EJ/yr)")

message('line #2749')
    fe.vars.woBunkers.bio <- c(   fe.vars.woBunkers.bio,
                                  "FE|w/o Non-energy Use|Liquids|+|Biomass (EJ/yr)")

message('line #2753')
    fe.vars.woBunkers.syn <- c(   fe.vars.woBunkers.syn,
                                  "FE|w/o Non-energy Use|Liquids|+|Hydrogen (EJ/yr)")
  }

  # add global values
message('line #2759')
  out <- mbind(out,dimSums(out,dim=1))
  # add other region aggregations
message('line #2762')
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))




  ### Further Variable Calculations ----



  # add per sector electricity share (for SDG targets)
message('line #2774')
  out <- mbind(out,
message('line #2776'),
    setNames(out[,,'FE|Buildings|+|Electricity (EJ/yr)'] / out[,,'FE|++|Buildings (EJ/yr)'] * 100, 'FE|Buildings|Electricity|Share (%)'),
message('line #2778'),
    setNames(out[,,'FE|Industry|+|Electricity (EJ/yr)']  / out[,,'FE|++|Industry (EJ/yr)']  * 100, 'FE|Industry|Electricity|Share (%)'),
message('line #2780'),
    setNames(out[,,'FE|Transport|+|Electricity (EJ/yr)'] / out[,,'FE|++|Transport (EJ/yr)'] * 100, 'FE|Transport|Electricity|Share (%)'),
message('line #2782'),
    setNames(out[,,'FE|+|Electricity (EJ/yr)'] / out[,,'FE (EJ/yr)'] * 100, 'FE|Electricity|Share (%)')
,message('line #2784')
  )
  # add per sector fuel share
message('line #2787')
  out <- mbind(out,
message('line #2789'),
   setNames(out[,,'FE|Buildings|Fuels (EJ/yr)'] / out[,,'FE|++|Buildings (EJ/yr)'] * 100, 'FE|Buildings|Fuels|Share (%)'),
message('line #2791'),
   setNames(out[,,'FE|Industry|Fuels (EJ/yr)']  / out[,,'FE|++|Industry (EJ/yr)']  * 100, 'FE|Industry|Fuels|Share (%)'),
message('line #2793'),
   setNames(out[,,'FE|Transport|Fuels (EJ/yr)'] / out[,,'FE|++|Transport (EJ/yr)'] * 100, 'FE|Transport|Fuels|Share (%)'),
message('line #2795'),
   setNames(out[,,'FE|Fuels (EJ/yr)'] / out[,,'FE (EJ/yr)'] * 100, 'FE|Fuels|Share (%)')
,message('line #2797')
  )

  ## specific energy use (FE per product/value added) ----
message('line #2801')
  if (all(indu_mod == 'subsectors',
          c('FE|Industry|+++|Cement (EJ/yr)',
            'Production|Industry|Cement (Mt/yr)',
            'FE|Industry|Steel|++|Primary (EJ/yr)',
            'Production|Industry|Steel|Primary (Mt/yr)',
            'FE|Industry|Steel|++|Secondary (EJ/yr)',
            'Production|Industry|Steel|Secondary (Mt/yr)',
            'FE|Industry|+++|Chemicals (EJ/yr)',
            'Value Added|Industry|Chemicals (billion US$2005/yr)',
            'FE|Industry|+++|Other Industry (EJ/yr)',
            'Value Added|Industry|Other Industry (billion US$2005/yr)') %>%
          `%in%`(getNames(out)))) {

message('line #2815')
    out <- mbind(
      out,
message('line #2818'),
      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( out[,,'FE|Industry|+++|Cement (EJ/yr)']
        / out[,,'Production|Industry|Cement (Mt/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Cement (GJ/t)'),

message('line #2826'),
      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( out[,,'FE|Industry|Steel|++|Primary (EJ/yr)']
        / out[,,'Production|Industry|Steel|Primary (Mt/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Primary Steel (GJ/t)'),

message('line #2834'),
      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( out[,,'FE|Industry|Steel|++|Secondary (EJ/yr)']
        / out[,,'Production|Industry|Steel|Secondary (Mt/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Secondary Steel (GJ/t)'),

message('line #2842'),
      setNames(
        ( out[,,'FE|Industry|+++|Chemicals (EJ/yr)']
        / out[,,'Value Added|Industry|Chemicals (billion US$2005/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Chemicals (MJ/US$2005)'),

message('line #2849'),
      setNames(
        ( out[,,'FE|Industry|+++|Other Industry (EJ/yr)']
        / out[,,'Value Added|Industry|Other Industry (billion US$2005/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Other Industry (MJ/US$2005)')
,message('line #2855')
    )
message('line #2857')
 }
message('line #2859')

  getSets(out)[3] <- "variable"
message('line #2862')
  return(out)
}
