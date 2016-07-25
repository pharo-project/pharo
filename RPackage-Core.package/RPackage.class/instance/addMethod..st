compiled method handling
addMethod: aCompiledMethod 
	"Add the method to the receiver as a defined method if the class is  defined in it, else as an extension."
	| methodClass |
	
	methodClass := aCompiledMethod methodClass. 
	(self includesClass: methodClass )
		ifTrue: [
			methodClass isMeta
				ifTrue: [
					(metaclassDefinedSelectors 
						at: methodClass theNonMetaClass name  
						ifAbsentPut: [ Set new ]) 
						add: aCompiledMethod selector]
				ifFalse: [
					(classDefinedSelectors 
						at: methodClass name 
						ifAbsentPut: [ Set new ]) 
						add: aCompiledMethod selector ] ]
		ifFalse:  [
			methodClass isMeta
				ifTrue: [
					(metaclassExtensionSelectors 
						at: methodClass theNonMetaClass name  
						ifAbsentPut: [ Set new ]) 
						add: aCompiledMethod selector.]
				ifFalse: [
					(classExtensionSelectors 
						at: methodClass name 
						ifAbsentPut: [ Set new ]) 
						add: aCompiledMethod selector].
				"we added a method extension so the receiver is an extending package of the class"
				self organizer registerExtendingPackage: self forClass: methodClass ].
					
	^ aCompiledMethod.