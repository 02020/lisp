(setq
  olistToolbar1 (list
		  '("保存" "^C^C_qsave" "SAVE.bmp")	
		  '("实测预测关联" "^C^C^P_SCYCGL2" "bros.ico")	'("导入老数据" "^C^C^P_DRXMLSJ" "1.bmp")
		  '("老数据测绘信息" "^C^C^P_SJBRCHXX" "2.bmp")	'("创建层工作区..." "^C^C^P_CJCGZQ" "3.bmp")
		  '("复制层工作区..." "^C^C^P_FZCGZQ" "3.bmp") '("提取关联老数据" "^C^C^P_TQGLYCZHS" "4.bmp")
		  '("编辑面积线" "^C^C^P_BJMJX" "编辑面积线.bmp")
		  '("提取未封闭阳台" "^C^C^P_TQMJX;3;" "未封闭阳台.bmp") '("提取封闭阳台" "^C^C^P_TQMJX;2;" "封闭阳台.bmp")
		  '("提取附属" "^C^C^P_TQMJX;38" "提取附属.bmp") '("提取半算附属" "^C^C^P_TQMJX;39" "提取半算附属.bmp")		  
		  )
  olistToolbar2 (list
		  '("导出接口数据..." "^C^C^P_DRXMMDB" "zip.ico")
		  '("生成房产成果图:SCZT" "^C^C^P_SCCGT;6;" "生成幢图.bmp")
		  '("生成自定义报表:SCBB" "^C^C^P_SCBB;" "生成报表.bmp")
		  '("查看楼盘表" "^C^C^P_CKLPB" "mr nice.ico")
		  ;'("生成分户图:SCFHT" "^C^C^P_SCCGT;0;" "生成分户图.bmp")
		  '("生成调查表(&H).." "^C^C^P_SCCGT;101; " "5.bmp")		  
		  '("下载成果图(&G)..." "^C^C^P_XZCGT  " "mr nice.ico")
		  '("成果导出..." "^C^C^P_DBEXPORT  " "导出.bmp") '("成果导入..." "^C^C^P_DBIMPORT  " "导入.bmp")
		  '("框选出复式分户图…" "^C^C^P_FSCT; " "复式.bmp") '("框选出复式分户图（多层）" "^C^C^P_FSCT2" "aah!.ico")
		  '("横向框选出分户图…" "^C^C^P_HXCT; " "户1.bmp") '("纵向框选出分户图…" "^C^C^P_ZXCT; " "户2.bmp")
		  )
    olistToolbar3 (list	
		  '("生成调查表(&H).." "^C^C^P_SCCGT;101; " "5.bmp")
		  '("批量出分层图（老数据）" "^C^C^P_CFCCT;2;" "ICON.ICO") '("生成层图WMF..." "^C^C^P_FCTWMF" "STAR.ICO")
		  '("批量出层草图…" "^C^C^P_CFCCT;1; " "草W.bmp") '("生成草图WMF..." "^C^C^P_FCCTWMF " "草.bmp") 
		  '("选择当前定界框..." "vff" "06.ico")	'("生成户图WMF..." "^C^C^P_FHTWMF " "户W.bmp")
		  )
  olistToolbar9 (list
		  '("选择当前定界框..." "vff" "06.ico")
		  '("布局<向上>..." "VF布局控制向上" "MARK0023.ICO")
		  '("布局控制..." "VF布局控制" "tv.ico")
		  '("布局<向下>..." "VF布局控制向下" "MARK0025.ICO")
		  '("生成户图WMF..." "^C^C^P_FHTWMF " "户W.bmp")
		  '("关闭..." "closesave " "X.ico")
		  
		)
  );setq


				
(defun c:Tool (/ oINT)
  (setq oINT 0)
  (if (menugroup "080工具栏")
    (command "menuunload" "080工具栏")
  )
  (#createtoolbar "w" "房测1" olistToolbar1)
  (#createtoolbar "a" "房测2" olistToolbar2)
  (#createtoolbar "a" "生成成果" olistToolbar3)
  (#createtoolbar "a" "布局控制" olistToolbar9)

  (princ "\n程序将自动创建 080工具栏 请稍后")
  (command "menuload" "080工具栏.mnu")
  (command "TOOLBAR" "房测2" "right" "")
  (command "TOOLBAR" "房测1" "right" "")
  (command "TOOLBAR" "布局控制" "F" "1250,100" "")
  (command "TOOLBAR" "生成成果" "F" "1250,160" "")

)

(setq  olistToolbarYS (list
			'("建立图层" "^C^C^P_MKlaYS" "RCDATA_16_VPPOLY")
			'("获取用途" "^C^C^P_YSHQYT" "RCDATA_16_VPCLIP")
			'("定义用途" "^C^C^P_YSEVWS" "RCDATA_16_VPONE")
			'("数据导出" "^C^C_YS6D" "RCDATA_16_VPOBJ")
			)
       )
(defun c:ToolYS (/ oINT)
  (setq oINT 0)
  (if (menugroup "080工具栏")
    (command "menuunload" "080工具栏")
  )
  (#createtoolbar "w" "预算" olistToolbarYS)
  (princ "\n程序将自动创建 080工具栏 请稍后")
  (command "menuload" "080工具栏.mnu")
 (command "TOOLBAR" "预算" "F" "1250,160" "")
  )


(defun c:tx (/ oINT)
  (princ "\n对象特性")
  (command "TOOLBAR" "对象特性" "S")
  (PRINC))
