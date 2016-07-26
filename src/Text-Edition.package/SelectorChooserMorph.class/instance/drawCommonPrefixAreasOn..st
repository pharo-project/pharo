drawing
drawCommonPrefixAreasOn: aCanvas
	| firstMenu firstMenuItem topLeft bottomLeft lastMenuItem |
	requestor ifNil: [^ self].
	prefix 
		ifNotNil: [ self choiceMenus 
			do: [:cm | self drawCommonPrefixAreasOn: aCanvas menu: cm]]
