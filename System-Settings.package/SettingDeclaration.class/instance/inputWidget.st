user interface
inputWidget
	"return the default widget for the input a the setting"
	| inputWidget xtraDialogWidget | 
	xtraDialogWidget := super inputWidget.
	inputWidget :=  self localInputWidgetSelector ifNotNil: [:localSel | self perform: localSel].
	(inputWidget isNil and: [self typeClass notNil])
		ifTrue: [inputWidget := (self inputWidgetSelectorInClass: self typeClass)
			ifNotNil: [:selector | self typeClass perform: selector with: self]].
	inputWidget
		ifNotNil: [(inputWidget respondsTo: #model:) 
		  ifTrue: [inputWidget model ifNil: [inputWidget model: self]]].
	xtraDialogWidget 
		ifNotNil: [inputWidget := inputWidget ifNil: [xtraDialogWidget] 
			ifNotNil: [ self theme  newRowIn: World for: {inputWidget. xtraDialogWidget}]].
	^ inputWidget 