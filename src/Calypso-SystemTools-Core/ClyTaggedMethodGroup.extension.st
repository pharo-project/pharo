Extension { #name : #ClyTaggedMethodGroup }

{ #category : #'*Calypso-SystemTools-Core' }
ClyTaggedMethodGroup >> decorateTableCell: anItemCellMorph of: groupItem [
	| myName |
	super decorateTableCell: anItemCellMorph of: groupItem.
	myName := name asLowercase.
	(myName beginsWith: 'private') ifTrue: [ 
		^anItemCellMorph definitionIcon: #protocolPrivateIcon ].
	
	(#('initial' 'class initial' 'releaz' 'releas' 'finali') 
		anySatisfy: [ :each | myName beginsWith: each ])
			ifTrue: [ anItemCellMorph definitionIcon: #protocolProtectedIcon]	
]
