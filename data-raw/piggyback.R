# usethis::use_github()
# piggyback::pb_release_create(tag = "datajud-api")
# piggyback::pb_release_create(tag = "datajud-pbi")

piggyback::pb_upload("data-raw/pbi/TJDFT_CN.csv", tag = "datajud-pbi")
piggyback::pb_upload("data-raw/pbi/TJMA_CN.csv", tag = "datajud-pbi")
piggyback::pb_upload("data-raw/pbi/TJMT_CN.csv", tag = "datajud-pbi")

piggyback::pb_upload("data-raw/tjdft.zip", tag = "datajud-api")
piggyback::pb_upload("data-raw/tjma.zip", tag = "datajud-api")
piggyback::pb_upload("data-raw/tjmt.zip", tag = "datajud-api")
