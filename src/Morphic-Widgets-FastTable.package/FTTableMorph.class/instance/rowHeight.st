accessing
rowHeight
	"This is the row height your rows will have. Cells answered in dataSource will be forced to have
	 this height number... We force it instead allowing lists to have any height because the logic to 
	 calculate rows becomes complicated. Possible, but complicated :)"
	^ rowHeight ifNil: [ rowHeight := self class defaultRowHeight ]