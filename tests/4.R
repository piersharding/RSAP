test.read_table <- function()
{
    conn <- RSAPConnect("sap.yml")
    parms <- list('DELIMITER' = '|',
                  'ROWCOUNT' = 2,
                  'QUERY_TABLE' = 'T000')
    res <- RSAPInvoke(conn, "RFC_READ_TABLE", parms)
    #str(res$ENTRIES)
    str(res$DATA)
    checkEquals(2, length(res$DATA$WA))
    checkTrue(RSAPClose(conn))
}
           
#test.deactivation <- function()
#{
# DEACTIVATED('Deactivating this test function')
#}
