EF_CFG_APPNAME:=tdebug
EF_CFG_USE_SEMIHOST:=y

COMPATIBLE_TARGETS:=bluepill

SOURCES+=$(wildcard $(EF_CFG_APP_DIR)/*.c)
