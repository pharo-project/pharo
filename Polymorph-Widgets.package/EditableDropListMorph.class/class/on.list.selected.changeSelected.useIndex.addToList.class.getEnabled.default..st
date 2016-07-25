as yet unclassified
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel useIndex: useIndex addToList: addToListSel class: aClass getEnabled: getEnabledSel default: aDefaultValue
	^ (super
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		useIndex: useIndex) convertTo: aClass;
		 getEnabledSelector: getEnabledSel;
		 addToListSel: addToListSel;
		 default: aDefaultValue;
		 yourself