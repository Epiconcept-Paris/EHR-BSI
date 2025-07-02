# Create a naming lookup
Name_Lookup <- data.table::data.table(
  earsval = c("PatientCounter", "Gender", "HospitalUnitType", "DateOfHospitalisation",
              "DateUsedForStatistics", "Pathogen",
              "ESBL", "ResultCarbapenemases", "ResultGradSign", "ResultGradValue",
              "ResultGradSIR", "ResultMICSign", "ResultMICValue", "ResultMICSIR",
              "ResultZoneSign", "ResultZoneValue", "ResultZoneSIR", "DiskLoad"),
  ehrval = c("PatientId", "Sex", "PatientSpecialty", "DateOfHospitalAdmission",
             "DateOfSpecimenCollection", "MicroorganismCode",
             "ResultESBL", "ResultCarbapenemase", "GradSusceptibilitySign", "GradValue",
             "GradSIR", "MICSusceptibilitySign", "MICValue", "MICSIR",
             "ZoneSusceptibilitySign", "ZoneValue", "ZoneSIR", "ZoneTestDiskLoad")
)

# Create a lookup for specialties - note the current mapping limitations
# between Emergency department, primary care, and mixed - unknown
Specialty_Lookup <- data.table::data.table(
  fromearsval = c("ED", "ICU", "INFECT", "INTMED", "OTH",  "OBGYN", "ONCOL", "PEDS", "PEDSICU", "PHC", "SURG", "UNK",  "URO"),
  ehrval = c("MIX", "ICUMIX", "MEDID", "MEDGEN", "OTH", "GOGYN", "MEDONCO", "PEDGEN", "ICUPED", "MIX", "SURGEN", "", "SURURO"),
  toearsval = c("UNK", "ICU", "INFECT", "INTMED", "OTH", "OBGYN", "ONCOL", "PEDS",  "PEDSICU", "UNK", "SURG", "UNK", "URO")
)

# Malta Unit Specialty Mapping
Malta_UnitSpecialty_Lookup <- data.table::data.table(
  malta_code = c("MIX", "MEDCARD", "MEDOTH", "MEDONCO", "MEDID", "MEDNEU", 
                 "SURCARD", "SURGEN", "SURBURN", "SURORTO", "SURURO", "SUROTH", 
                 "SURORTR", "SURNEU", "SURENT", "SURPLAS", "ICUMED", "ICUMIX", 
                 "ICUSPEC", "ICUNEO", "PEDGEN", "LTC", "GOOBS", "GOBAB", "GOGYN", 
                 "O", "Other"),
  generic_code = c("MIX", "MED", "MED", "MED", "MED", "MED", 
                 "SUR", "SUR", "SUR", "SUR", "SUR", "SUR", 
                 "SUR", "SUR", "SUR", "SUR", "ICU", "ICU", 
                 "ICU", "NEO", "PED", "LTC", "GO", "GO", "GO", 
                 "OTH", "OTH")
)

# Malta Outcome Mapping
Malta_Outcome_Lookup <- data.table::data.table(
  malta_code = c("VWDISC-DEC", "DISC-DECX", "CT-DECX"),
  generic_code = c("D", "D", "D")
)

# Malta Hospital Type Mapping
Malta_HospType_Lookup <- data.table::data.table(
  malta_hosptype = c("GGH", "MDH", "OC"),
  hosptype_code = c("SEC", "TERT", "SPEC")
)

# Estonia Mechanism Resistance Mapping
Estonia_MecRes_Lookup <- data.table::data.table(
  resistance_type = c(rep("ResultESBL", 8), rep("ResultCarbapenemase", 3), rep("ResultPCRmec", 2)),
  resistance_value = c("ESBL-A", "ESBL-A positiivne", "ESBL-M", "ESBL-M positiivne", "ESBL-A/M",
                       "Laiendatud toimespektriga beet", "Laiendatud toimespektriga beetalaktamaasi A mitteprodutseeriv tüvi",
                       "Beeta-laktamaas positiivne", "OXA-48 tüüpi", "OXA-48 tüüpi positiivne",
                       "OXA-48 tüüpi karbapenemaasi produtseeriv tüvi", "MRSA", "PVL")
)

# Estonia Resistance Recoding
Estonia_ResRecode_Lookup <- data.table::data.table(
  estonia_result = c("Laiendatud toimespektriga beet", "VRE", "Negatiivne", "OXA-48 tüüpi positiivne",
                     "Positiivne", "ESBL-A", "OXA-48 tüüpi", "ESBL-A positiivne", "ESBL-M positiivne",
                     "MRSA", "Beeta-laktamaas positiivne", "ESBL-M", "MDR", "ESBL-A/M", "PVL",
                     "OXA-48 tüüpi karbapenemaasi produtseeriv tüvi", "Laiendatud toimespektriga beetalaktamaasi A mitteprodutseeriv tüvi"),
  generic_result = c("Positive", "Positive", "Negative", "Positive", "Positive", "Positive", "Positive",
                      "Positive", "Positive", "Positive", "Positive", "Positive", "Positive", "Positive",
                      "Positive", "Positive", "Negative")
)

# Estonia to English Antibiotic Names
Estonia_Ab_EST2ENG_Lookup <- data.table::data.table(
  estonia_name = c("Amoksitsilliin + klavulaanhape", "Amoksitsilliin + klavulaanhape (parenteraalne)",
                   "Amoksitsilliin + klavulaanhape (suukaudne)", "Amoksitsilliin+klavulaanhape",
                   "Ampitsilliin", "Ampitsilliin + sulbaktaam", "Ampitsilliin+sulbaktaam",
                   "Piperatsilliin + tasobaktaam", "Piperatsilliin+tasobaktaam", "Penitsilliin",
                   "Penitsilliin G", "Penitsilliin madalas kontsentratsioonis", "Tsefotaksiim",
                   "Tsefotaksiim madalas kontsentratsioonis", "Tseftasidiim", "Tseftasidiim + avibaktaam",
                   "Tsefepiim", "Tsefiderokool", "Tseftriaksoon", "Tsefoksitiin", "Tsefuroksiim",
                   "Tsefuroksiim (parenteraalne)", "Tsefuroksiim (suukaudne)", "Meropeneem",
                   "Imipeneem", "Ertapeneem", "Tseftolosaan + tasobaktaam", "Tsiprofloksatsiin",
                   "Levofloksatsiin", "Moksifloksatsiin", "Norfloksatsiin", "Gentamütsiin",
                   "Gentamütsiini kõrges kontsentratsioonis", "Amikatsiin", "Tobramütsiin",
                   "Erütromütsiin", "Klaritromütsiin", "Asitromütsiin", "Klindamütsiin",
                   "Kinupristiin + dalfopristiin", "Linesoliid", "Vankomütsiin", "Teikoplaniin",
                   "Daptomütsiin", "Tetratsükliin", "Tigetsükliin", "Rifampitsiin", "Oksatsilliin",
                   "Fusidiin", "Fosfomütsiin", "Trimetoprim + sulfametoksasool", "Trimetoprim+sulfametoksasool",
                   "Nitrofurantoiin", "Metronidasool", "Kolistiin", "Mupirotsiin", "Anidulafungiin",
                   "Flukonasool", "Itrakonasool", "Kaspofungiin", "Posakonasool", "Vorikonasool",
                   "Mikafungiin", "5-Fluorotsütosiin", "Amfoteritsiin", "Amfoteritsiin B"),
  english_name = c("Amoxicillin + clavulanic acid", "Amoxicillin + clavulanic acid (parenteral)",
                   "Amoxicillin + clavulanic acid (oral)", "Amoxicillin + clavulanic acid",
                   "Ampicillin", "Ampicillin + sulbactam", "Ampicillin + sulbactam",
                   "Piperacillin + tazobactam", "Piperacillin + tazobactam", "Penicillin",
                   "Penicillin G", "Penicillin G (low concentration)", "Cefotaxime",
                   "Cefotaxime (low concentration)", "Ceftazidime", "Ceftazidime + avibactam",
                   "Cefepime", "Cefiderocol", "Ceftriaxone", "Cefoxitin", "Cefuroxime",
                   "Cefuroxime (parenteral)", "Cefuroxime (oral)", "Meropenem",
                   "Imipenem", "Ertapenem", "Ceftolozane + tazobactam", "Ciprofloxacin",
                   "Levofloxacin", "Moxifloxacin", "Norfloxacin", "Gentamicin",
                   "Gentamicin (high concentration)", "Amikacin", "Tobramycin",
                   "Erythromycin", "Clarithromycin", "Azithromycin", "Clindamycin",
                   "Quinupristin + dalfopristin", "Linezolid", "Vancomycin", "Teicoplanin",
                   "Daptomycin", "Tetracycline", "Tigecycline", "Rifampicin", "Oxacillin",
                   "Fusidic acid", "Fosfomycin", "Trimethoprim + sulfamethoxazole", "Trimethoprim + sulfamethoxazole",
                   "Nitrofurantoin", "Metronidazole", "Colistin", "Mupirocin", "Anidulafungin",
                   "Fluconazole", "Itraconazole", "Caspofungin", "Posaconazole", "Voriconazole",
                   "Micafungin", "Flucytosine", "Amphotericin B", "Amphotericin B")
)

# English to HAI Format Antibiotic Names
Estonia_Ab_ENG2HAI_Lookup <- data.table::data.table(
  english_name = c("No antimicrobial susceptibility data available", "Amoxicillin + clavulanic acid",
                   "Amoxicillin + clavulanic acid (parenteral)", "Amoxicillin + clavulanic acid (oral)",
                   "Ampicillin + sulbactam", "Piperacillin + tazobactam", "Ceftazidime + avibactam",
                   "Ceftolozane + tazobactam", "Penicillin", "Penicillin G", "Penicillin G (low concentration)",
                   "Ampicillin", "Amoxicillin", "Oxacillin", "Methicillin", "Cloxacillin", "Dicloxacillin",
                   "Cefazolin", "Cefalotin", "Cefuroxime", "Cefuroxime (parenteral)", "Cefuroxime (oral)",
                   "Cefoxitin", "Cefotaxime", "Cefotaxime (low concentration)", "Ceftriaxone",
                   "Cephalosporins, third generation", "Ceftazidime", "Cefepime", "Cefiderocol",
                   "Cephalosporins, fourth generation", "Imipenem", "Meropenem", "Doripenem", "Ertapenem",
                   "Imipenem/relebactam", "Meropenem/vaborbactam", "Carbapenems", "Ciprofloxacin",
                   "Levofloxacin", "Moxifloxacin", "Norfloxacin", "Nalidic acid", "Ofloxacin",
                   "Gentamicin", "Gentamicin (high concentration)", "Amikacin", "Tobramycin", "Netilmicin",
                   "Plazomicin", "Erythromycin", "Clarithromycin", "Azithromycin", "Clindamycin",
                   "Quinupristin + dalfopristin", "Vancomycin", "Teicoplanin", "Glycopeptides", "Daptomycin",
                   "Tetracycline", "Eravacycline", "Tigecycline", "Linezolid", "Rifampicin", "Fosfomycin",
                   "Fusidic acid", "Colistin", "Polymyxin B", "Nitrofurantoin", "Metronidazole", "Mupirocin",
                   "Temocillin", "Piperacillin", "Piperacillin or ticarcillin", "Sulbactam",
                   "Trimethoprim + sulfamethoxazole", "Amphotericin", "Amphotericin B", "Fluconazole",
                   "Itraconazole", "Voriconazole", "Posaconazole", "Ketoconazole", "Anidulafungin",
                   "Caspofungin", "Micafungin", "Flucytosine", "5-Fluorocytosine"),
  generic_name = c("No antimircrobial susceptibility data available", "Amoxicillin/Clavulanic Acid",
               "Amoxicillin/Clavulanic Acid", "Amoxicillin/Clavulanic Acid", "Sulbactam",
               "Piperacillin/Tazobactam", "Ceftazidime/avibactam", "Ceftolazone/tazobactam",
               "Penicillin", "Penicillin", "Penicillin", "Ampicillin", "Amoxicillin", "Oxacillin",
               "Methicillin", "Cloxacillin", "Dicloxacillin", "Cephalosporins, first generation (cefalotin/cefazolin)",
               "Cephalosporins, first generation (cefalotin/cefazolin)", "Cephalosporins, second generation (cefuroxim/cefamandole/cefoxitin)",
               "Cephalosporins, second generation (cefuroxim/cefamandole/cefoxitin)", "Cephalosporins, second generation (cefuroxim/cefamandole/cefoxitin)",
               "Cefoxitin", "Cefotaxime", "Cefotaxime", "Ceftriaxone", "Cephalosporins, third generation (cefotaxime/ceftriaxone)",
               "Ceftazidime", "Cefepime", "Cefiderocol", "Cephalosporins, fourth generation (cefepime/cefpirome)",
               "Imipenem", "Meropenem", "Doripenem", "Ertapenem", "Imipenem/relebactam", "Meropenem/vaborbactam",
               "Carbapenems (imipenem, meropenem, doripenem)", "Ciprofloxacin", "Levofloxacin", "Moxifloxacin",
               "Norfloxacin", "Nalidic acid", "Ofloxacin", "Gentamicin", "Gentamicin-High", "Amikacin",
               "Tobramycin", "Netilmicin", "Plazomicin", "Erythromycin", "Clarithromycin", "Azithromycin",
               "Clindamycin", "Quinupristin/Dalfopristin", "Vancomycin", "Teicoplanin", "Glycopeptides (vancomycin/teicoplanin)",
               "Daptomycin", "Tetracyclin", "Eravacycline", "Tigecyclin", "Linezolid", "Rifampin", "Fosfomycin",
               "Fusidic acid", "Colistin", "Polymyxin B", "Nitrofurantoin", "Metronidazole", "Mupirocin",
               "Temocillin", "Piperacillin", "Piperacillin or ticarcillin", "Sulbactam", "Trimethoprim/Sulfamethoxazole (cotrimoxazole)",
               "Amphotericin B", "Amphotericin B", "Fluconazole", "Itraconazole", "Voriconazole", "Posaconazole",
               "Ketoconazole", "Anidulafungin", "Caspofungin", "Micafungin", "Flucytosine (5 - fluorocytosine)",
               "Flucytosine (5 - fluorocytosine)")
)


# Malta Pathogen Name to Code Mapping
Malta_PathogenCode_Lookup <- data.table::data.table(
  malta_pathogen_name = c("Abiotrophia defectiva", "Achromobacter (Alc.) xylosoxidans ss. xylosoxidans",
                          "Acinetobacter baumannii", "Acinetobacter lwoffii", "Acinetobacter pittii",
                          "Acinetobacter sp.", "Aeromonas veronii", "Anaerobic gram negative rods",
                          "Anaerobic gram positive rods", "Bacteroides fragilis", "Bacteroides ovatus",
                          "Bacteroides thetaiotaomicron", "Bacteroides vulgatus", "Bifidobacterium sp.",
                          "Candida albicans", "Candida glabrata", "Candida krusei", "Candida parapsilosis",
                          "Candida tropicalis", "Citrobacter braakii", "Citrobacter freundii",
                          "Citrobacter koseri (diversus)", "Clostridium perfringens", "Clostridium septicum",
                          "Enterobacter aerogenes", "Enterobacter cloacae", "Enterobacter hormaechei",
                          "Enterococcus faecalis", "Enterococcus faecium", "Escherichia coli",
                          "Fusobacterium nucleatum", "Gemella morbillorum", "Gram negative rods",
                          "Gram positive cocci", "Gram positive rods", "Gram positive bacteria",
                          "Haemophilus influenzae", "Hafnia alvei", "Klebsiella oxytoca", "Klebsiella pneumoniae",
                          "Lactococcus garvieae", "Moraxella (Branh.) catarrhalis", "Moraxella osloensis",
                          "Morganella morganii", "Neisseria subflava", "Pantoea agglomerans",
                          "Proteus mirabilis", "Proteus penneri", "Proteus vulgaris", "Providencia rettgeri",
                          "Providencia stuartii", "Pseudomonas aeruginosa", "Pseudomonas putida",
                          "Psychrobacter sp.", "Saccharomyces cerevisiae", "Salmonella sp.",
                          "Serratia marcescens", "Staphylococcus aureus", "Staphylococcus",
                          "Streptococcus agalactiae", "Streptococcus dysgalactiae", "Streptococcus pneumoniae",
                          "Streptococcus pyogenes", "Streptococcus sp.", "Actinomyces sp.",
                          "Aerococcus urinae", "Aerococcus viridans", "Aeromonas punctata ss. punctata",
                          "Aggregatibacter aphrophilus", "Alcaligenes faecalis (odorans) (CDC VI)",
                          "Arthrobacter sp.", "Bacillus coagulans", "Bacillus sp.", "Bacteroides distasonis",
                          "Bordetella sp.", "Brevibacterium casei", "Brevibacterium sp.",
                          "Brevundimonas (Pseudo.) diminuta (CDC Ia)", "Brucella melitensis",
                          "Burkholderia stabilis", "Campylobacter jejuni ss. jejuni", "Candida lusitaniae",
                          "Citrobacter youngae", "Corynebacterium afermentans", "Corynebacterium amycolatum",
                          "Corynebacterium auris", "Corynebacterium glucuronolyticum",
                          "Corynebacterium mucifaciens", "Corynebacterium sp.", "Corynebacterium striatum",
                          "Corynebacterium urealyticum", "Corynebacterium xerosis", "Cryptococcus neoformans",
                          "Dermabacter hominis", "Enterococcus gallinarum", "Escherichia vulneris",
                          "Fungus", "Fusobacterium mortiferum", "Gemella sp.", "Geotrichum capitatum",
                          "Kocuria kristinae", "Kocuria rosea", "Kocuria sp.", "Lactobacillus acidophilus",
                          "Lactobacillus brevis", "Lactobacillus casei", "Leclercia adecarboxylata",
                          "Leuconostoc lactis", "Leuconostoc mesenteroides", "Lysinibacillus sp.",
                          "Microbacterium sp.", "Micrococcus luteus", "Moraxella sp.",
                          "Mycobacterium tuberculosis complex", "Neisseria mucosa", "Paenibacillus sp.",
                          "Pasteurella multocida", "Peptostreptococcus asaccharolyticus",
                          "Propionibacterium acnes", "Pseudomonas luteola", "Pseudomonas oryzihabitans",
                          "Pseudomonas sp.", "Pseudomonas stutzeri (CDC Vb-1)",
                          "Pseudopropionibacterium propionicum", "Roseomonas cervicalis",
                          "Rothia mucilaginosus", "Rothia sp.", "Ruminococcus sp.", "Salmonella Enteritidis",
                          "Salmonella Infantis", "Sphingomonas (Pseudo.) paucimobilis (IIk-1)",
                          "Staphylococcus capitis", "Staphylococcus epidermidis", "Staphylococcus haemolyticus",
                          "Staphylococcus hominis", "Staphylococcus pseudintermedius",
                          "Stenotrophomonas (Xantho.) maltophilia", "Streptococcus anginosus",
                          "Staphylococcus pasteuri", "Streptococcus constellatus",
                          "Streptococcus dysgalactiae ss. equisimilis", "Streptococcus gallolyticus ss. gallolyticus",
                          "Streptococcus gallolyticus ss. pasteurianus", "Streptococcus gordonii",
                          "Streptococcus intermedius", "Streptococcus mitis", "Streptococcus mutans",
                          "Streptococcus oralis", "Streptococcus parasanguinis", "Streptococcus salivarius",
                          "Streptococcus sanguinis", "Streptococcus viridans", "Trichosporon asahii",
                          "Veillonella sp.", "Yeast"),
  microorganism_code = c("113714003", "423897003", "91288006", "83088009", "698244000", "7757008",
                         "9281006", "243386003", "243398008", "55247009", "86936008", "34236001",
                         "52798008", "5912005", "53326005", "444877006", "76995003", "61302002",
                         "47885008", "114262000", "6265002", "114264004", "8331005", "33101000",
                         "62592009", "14385002", "114454006", "78065002", "90272000", "112283007",
                         "32763000", "113765006", "87172008", "59206002", "83514008", "8745002",
                         "44470000", "76694001", "40886007", "56415008", "27716002", "24226003",
                         "72401008", "243301005", "77700001", "115015008", "73457008", "45298005",
                         "45834001", "14196002", "1445008", "52499004", "68608003", "34412002",
                         "58296003", "27268008", "33522002", "3092008", "65119002", "43492007",
                         "40812000", "9861002", "80166006", "58800005", "40560008", "243230001",
                         "78803006", "TBC_47", "TBC_46", "TBC_45", "56214009", "TBC_44", "44762009",
                         "TBC_43", "TBC_42", "6405003", "3603008", "TBC_41", "TBC_40", "TBC_39",
                         "TBC_38", "TBC_37", "TBC_36", "113608003", "113611002", "413925000",
                         "413927008", "413929006", "77086004", "63815007", "113613004", "27101006",
                         "TBC_35", "413977006", "TBC_34", "TBC_33", "TBC_32", "TBC_31", "TBC_30",
                         "TBC_29", "113772007", "113773002", "414328004", "TBC_28", "TBC_27",
                         "TBC_26", "TBC_25", "TBC_24", "TBC_23", "TBC_22", "114199008", "48299003",
                         "TBC_21", "TBC_20", "TBC_19", "114086007", "TBC_18", "TBC_17", "TBC_16",
                         "TBC_15", "TBC_14", "TBC_13", "TBC_12", "TBC_11", "113711006", "416544005",
                         "90163001", "TBC_10", "TBC_9", "TBC_8", "TBC_7", "40347003", "60875001",
                         "83452006", "44827007", "TBC_6", "TBC_5", "44304009", "103495006",
                         "76199005", "TBC_4", "421544007", "421253004", "113986004", "62170002",
                         "57997003", "214001", "19870004", "113990002", "39888004", "113993000",
                         "31703004", "TBC_3", "TBC_2", "TBC_1")
)

usethis::use_data(Name_Lookup, Specialty_Lookup, Malta_UnitSpecialty_Lookup, Malta_Outcome_Lookup, 
                  Malta_HospType_Lookup, Malta_PathogenCode_Lookup, Estonia_MecRes_Lookup, Estonia_ResRecode_Lookup, 
                  Estonia_Ab_EST2ENG_Lookup, Estonia_Ab_ENG2HAI_Lookup, 
                  internal = FALSE, overwrite = TRUE)
