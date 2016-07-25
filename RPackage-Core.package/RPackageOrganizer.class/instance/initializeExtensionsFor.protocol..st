initialization-data
initializeExtensionsFor: aBehavior protocol: aProtocol
	| package protocolName nonTraitMethods|
	
	protocolName := (aProtocol name allButFirst) trimBoth.
	package := self packageMatchingExtensionName: protocolName.
	package ifNil: [
		package := self basicRegisterPackage: (RPackage named: protocolName) ].
	nonTraitMethods := aProtocol methods 
		select: [ :eachSelector | (aBehavior >> eachSelector) origin = aBehavior ].
	nonTraitMethods ifEmpty:[^ self].
	self registerExtendingPackage: package forClass: aBehavior.
	nonTraitMethods
		do: [ :eachSelector | package addMethod: aBehavior >> eachSelector ]