widget API
menu: aMenu shifted: aBoolean

	^ (self model menu: aMenu shifted: aBoolean) 
		ifNotNil: [ :menuModel | menuModel buildWithSpecAsPopup ]