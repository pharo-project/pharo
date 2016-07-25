adding
findReferencesIn: aMethod for: aPackage
	| package compiledMethod |

	compiledMethod := aMethod compiledMethod.
	^ compiledMethod
		ifNotNil: [ 
			compiledMethod literals allButLast
				do: [ :literal | 
					(self isReference: literal)
						ifTrue: [ 
							package := self packageForBehavior: literal value.
							self addPackage: package.
							aPackage
								add:
									((DAReferenceDependency from: aPackage to: package)
										theClass: aMethod actualClass;
										selector: aMethod method selector asSymbol;
										reference: literal value;
										method: aMethod;
										yourself) ] ] ]