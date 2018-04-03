;;初始化变量
(setq
configLayerList
(list '("BM" "BM" 250)	'("CF" "CF" 3) '("DC" "DC" 1) '("DS" "DS" 3) '("DT" "DT" 3)
      '("DX" "DX" 3) '("GB" "GB" 3) '("GD" "GD" 1) '("GG" "GG" 1) '("GN" "GN" 30)
      '("GY" "GY" 250)	'("JK" "JK" 3) '("JY" "JY" 3) '("JZ" "JZ" 5) '("LD" "LD" 1)
      '("LH" "LH" 5) '("LN" "LN" 30)   '("LT" "LT" 3) '("MQ" "MQ" 6) '("PS" "PS" 5)
      '("QK" "QK" 3) '("QQ" "QQ" 250)  '("RC" "RC" 3) '("RG" "RG" 30)
      '("RS" "RS" 30)	'("RT" "RT" 3) '("SS" "SS" 5) '("SY" "SY" 250)
      '("SZ" "SZ" 3) '("TR" "TR" 6) '("TT" "TT" 3) '("TW" "TW" 3) '("TX" "TX" 3)
      '("WS" "WS" 42)	'("WT" "WT" 3) '("XF" "XF" 5) '("XH" "XH" 1) '("XS" "XS" 5)
      '("YD" "YD" 3) '("YH" "YH" 6) '("YM" "YM" 250)  '("YQ" "YQ" 250)
      '("YS" "YS" 42)	'("YY" "YY" 250)  '("ZF" "ZF" 3) '("ZH" "ZH" 250)
      '("ZL" "ZL" 1) '("ZQ" "ZQ" 30)   '("ZS" "ZS" 5) '("ZW" "ZW" 3)
     )

  configNodeSortList
   (list '("SS" "JS") ;增加
	 '("FZQ" "DX")	   '("XS" "DX")	     '("DHT" "DX")     '("PXJ" "DX")	 '("JX" "DX")	   '("RJD" "DX")
	 '("JSQ" "DX")	   '("JJ" "JS")	     '("JFJ" "JS")     '("SB" "JS")	 '("SBJ" "JS")	   '("PQ" "JS")
	 '("PQJ" "JS")	   '("PW" "JS")	     '("PWJ" "JS")     '("XFS" "JS")	 '("XFJ" "JS")	   '("JF" "JS")
	 '("SYJ" "JS")	   '("BZ" "JS")	     '("JSK" "JS")     '("CDZ" "JS")	 '("CSK" "JS")	   '("CLJ" "JS")
	 '("SH" "JS")	   '("SSS" "JS")     '("FMK" "JS")     '("GD" "JS")	 '("YJ" "PS")	   '("YB" "PS")
	 '("WB" "PS")	   '("CQJ" "PS")     '("DSJ" "PS")     '("ZM" "PS")	 '("WJ" "PS")	   '("PJ" "PS")
	 '("HFC" "PS")	   '("MFJ" "RQ")     '("MF" "RQ")      '("NSG" "RQ")	 '("TYX" "RQ")	   '("TYZ" "RQ")
	 '("MJ" "RQ")	   '("BCQ" "RQ")     '("JLB" "RQ")     '("TRJ" "RQ")	 '("YJX" "RQ")	   '("RFJ" "RL")
	 '("RF" "RL")	   '("RJ" "RL")	     '("XSF" "RL")     '("PCK" "RL")	 '("PCJ" "RL")	   '("GYJ" "GY")
	 '("SYJJ" "GY")	   '("GF" "GY")	     '("GFJ" "GY")     '("ZHJ" "ZH")	 '("BMJ" "BM")	   '("BDZ" "DL")
	 '("JYZ" "JS")	   '("CDC" "JS")     '("CPZ" "RQ")     '("5T" "PS")	 '("ZZ" "DL")	   '("BJ" "DL")
	 '("BC" "DL")	   '("FZ" "DL")	     '("YLK" "DL")     '("ZX" "DL")	 '("CD" "DL")	   '("FP" "DL")
	 '("PX" "DL")	   '("JBD" "DL")     '("SG" "DL")      '("BS" "DL")	 '("ZD" "DL")	   '("TBD" "DL")
	 '("DT" "JS")	   '("3T" "JS")	     '("4T" "JS")      '("QD" "PS")	 '("PXD" "PS")	   '("BP" "RQ")
	 '("BDF" "DL")	   '("BYQ" "DL")     '("DJ" "DL")      '("KZ" "DL")	 '("DG" "DL")	   '("XG" "DL")
	 '("Πg" "DL")	   '("TT" "DL")	     '("XB" "DL")      '("HWG" "DL")	 '("FX" "DL")	   '("LRK" "DL")
	 '("LSK" "DL")	   '("XHD" "DL")     '("RK" "DX")      '("SK" "DX")
	)
  configNodeList
   (list '("ZD" "DL" 1 "路灯" "电力")	     '("ZM" "DL" 1 "" "电力")	 '("SS" "JS" 5 "上水" "给水")
	 
	 '("PS" "JS" 5 "配水" "给水")	     '("ZS" "JS" 5 "中水" "给水")	 '("XS" "JS" 5 "循环水" "给水")
	 '("XF" "JS" 5 "专用消防水" "给水")  '("LH" "JS" 5 "绿化水" "给水")	 '("JZ" "JS" 5 "直饮水" "给水")
	 '("YS" "PS" 42 "雨水" "排水")	     '("WS" "PS" 42 "污水" "排水")	 '("GD" "DL" 1 "供电" "电力")
	 '("LD" "DL" 1 "路灯" "电力")	     '("XH" "DL" 1 "信号灯" "电力")	 '("DC" "DL" 1 "电车" "电力")
	 '("GG" "DL" 1 "广告灯" "电力")	     '("ZL" "DL" 1 "直流专用" "电力")	 '("DX" "TX" 3 "电信" "通信")
	 '("WT" "TX" 3 "网通" "通信")	     '("YD" "TX" 3 "移动" "通信")	 '("LT" "TX" 3 "联通" "通信")
	 '("DT" "TX" 3 "电通" "通信")	     '("TT" "TX" 3 "铁通" "通信")	 '("GB" "TX" 3 "广播电缆" "通信")
	 '("JY" "TX" 3 "军用电缆" "通信")    '("DS" "TX" 3 "有线电视" "通信")	 '("JK" "TX" 3 "交通监控" "通信")
	 '("TW" "TX" 3 "天网" "通信")	     '("QK" "TX" 3 "其它监控" "通信")	 '("CF" "TX" 3 "城发管网" "通信")
	 '("RC" "TX" 3 "绕城通信" "通信")    '("RT" "TX" 3 "热力通信" "通信")	 '("ZW" "TX" 3 "专用网络" "通信")
	 '("ZF" "TX" 3 "政府专网" "通信")    '("MQ" "RQ" 6 "煤气" "燃气")	 '("TR" "RQ" 6 "天然气" "燃气")
	 '("YH" "RQ" 6 "液化气" "燃气")	     '("RS" "RL" 30 "热水" "热力")	 '("ZQ" "RL" 30 "蒸汽" "热力")
	 '("LN" "RL" 30 "冷凝水" "热力")     '("GN" "RL" 30 "供暖" "热力")	 '("RG" "ZH" 30 "热力沟" "热力")
	 '("GY" "GY" 250 "工业" "工业")	     '("QQ" "GY" 250 "氢气管道" "工业")	 '("YQ" "GY" 250 "氧气管道" "工业")
	 '("YY" "GY" 250 "乙炔" "工业")	     '("SY" "GY" 250 "石油" "工业")	 '("ZH" "ZH" 250 "综合管沟" "综合管沟")
	 '("BM" "BM" 250 "不明管线" "不明管线")					 '("YM" "ZH" 250 "预埋" "预埋")
	 '("TX" "TX" 3 "共通" "通信")  '("TV" "TX" 3 "共通" "通信")
	)
  configPoint
   (list '(1 "X坐标")
'(2 "Y坐标")
'(3 "管线性质")
'(4 "外业点号")
'(5 "节点性质")
'(6 "地面高程")
'(7 "井深")
'(8 "井脖高")
'(9 "井室规格")
'(10 "井盖材质")
'(11 "井盖规格")
'(12 "附属物规格")
'(13 "管点材质")
'(14 "所在道路")
'(15 "所含管类")
'(16 "备注")


	)
configLine (list '(1 "管线性质")
'(2 "埋设方式")
'(3 "起点点号")
'(4 "终点点号")
'(5 "起点埋深")
'(6 "终点埋深")
'(7 "断面尺寸")
'(8 "材质")
'(9 "保温材质")
'(10 "保护材质")
'(11 "压力")
'(12 "介质")
'(13 "电压")
'(14 "总孔数")
'(15 "已用孔数")
'(16 "电缆根数")
'(17 "权属单位")
'(18 "所含管类")
'(19 "所在道路")
'(20 "管段位置")
'(21 "备注")
)







  configArrowColor    155	  ;箭头颜色
  configPointCircle    1	  ;点圆半径
  configLayerTemp    "080_0"
  configBlockAttrId    (list "id" "" "" "") 
  configExportDataFilePath    (list "d:\\020\\pipelineNode.txt" "d:\\020\\pipelineDiameter.txt")
  configDiameterFontSize    0.6
)

(princ)




;;;
;;;
;;;(setq ADOLISP_ADODLLPath
;;;       (findfile "c:\\program files\\common files\\system\\ado\\msado15.dll")
;;;)
;;;
;;;(vlax-import-type-library
;;;  :tlb-filename	ADOLISP_ADODLLPath :methods-prefix "ADOMethod-"	:properties-prefix "ADOProperty-" :constants-prefix
;;;  "ADOConstant-")
