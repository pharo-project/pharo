*Polymorph-Widgets
removeCloseBox
	"Remove the close box."

	closeBox ifNotNil: [closeBox delete. closeBox := nil]