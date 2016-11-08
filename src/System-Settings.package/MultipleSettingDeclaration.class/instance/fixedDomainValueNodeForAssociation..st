user interface
fixedDomainValueNodeForAssociation: anAssociation 
	| s |
	^ (s := (SettingDeclaration new name: anAssociation key translated))
		target: s;
		selector: #default;
		default: anAssociation value