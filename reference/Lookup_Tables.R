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
  short_code = c("MIX", "MED", "MED", "MED", "MED", "MED", 
                 "SUR", "SUR", "SUR", "SUR", "SUR", "SUR", 
                 "SUR", "SUR", "SUR", "SUR", "ICU", "ICU", 
                 "ICU", "NEO", "PED", "LTC", "GO", "GO", "GO", 
                 "OTH", "OTH")
)

# Malta Outcome Mapping
Malta_Outcome_Lookup <- data.table::data.table(
  malta_outcome = c("VWDISC-DEC", "DISC-DECX", "CT-DECX"),
  outcome_code = c("D", "D", "D")
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
  standard_result = c("Positive", "Positive", "Negative", "Positive", "Positive", "Positive", "Positive",
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
  hai_name = c("No antimircrobial susceptibility data available", "Amoxicillin/Clavulanic Acid",
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

usethis::use_data(Name_Lookup, Specialty_Lookup, Malta_UnitSpecialty_Lookup, Malta_Outcome_Lookup, 
                  Malta_HospType_Lookup, Estonia_MecRes_Lookup, Estonia_ResRecode_Lookup, 
                  Estonia_Ab_EST2ENG_Lookup, Estonia_Ab_ENG2HAI_Lookup, 
                  internal = FALSE, overwrite = TRUE)
