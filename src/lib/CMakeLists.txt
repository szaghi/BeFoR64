ADD_LIBRARY(BeFoR64
        befor64.F90
        befor64_pack_data_m.F90
        )
target_link_libraries(BeFoR64 PENF)
