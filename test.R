library('RSAP')
conn = RSAPConnect("sap.yml")
#conn = RSAPConnect(ashost="nplhost", sysnr="42", client="001", user="developer", passwd="developer", lang="EN", trace="1", lcheck="1")

print(conn)
info = RSAPGetInfo(conn)
print(info)

parms <- list('BYPASS_BUFFER' = 'X',
              'MAX_ENTRIES' = 50,
              'TABLE_NAME' = 'T005')
res = RSAPInvoke(conn, "RFC_GET_TABLE_ENTRIES", parms)
print(res)
print(res$ENTRIES)

str(res)
str(res$ENTRIES)

rc = RSAPClose(conn)
print(rc)
