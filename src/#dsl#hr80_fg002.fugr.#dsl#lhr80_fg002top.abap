FUNCTION-POOL /dsl/hr80_fg002 MESSAGE-ID hrpay99impexp.
* INCLUDE /DSL/LHR80_FG002D...               " Local class definition
*INCLUDE rpcbufferhandler.

TYPE-POOLS: abap,
            hrpay.


TABLES: pcl1,
        pcl2.
INCLUDE rpc2cd00.            " Payroll İnludes
INCLUDE rpc2rxx0.            " Payroll İnludes
INCLUDE rpc2rx00.            " Payroll İnludes
INCLUDE rpppxd00.            " Payroll İnludes
INCLUDE rpppxd10.            " Payroll İnludes
INCLUDE rpppxm00.            " Payroll İnludes
INCLUDE pc2rxtr0.            " Payroll İnludes

INCLUDE up50qdat.
INCLUDE rpppxi19.
