user interface
fixedDomainValueNodeForObject: anObject 
	| s |
	^ (s := (SettingDeclaration new name: anObject asString))
		target: s;
		selector: #default;
		default: anObject