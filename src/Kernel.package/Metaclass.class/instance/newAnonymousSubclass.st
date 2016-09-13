instance creation
newAnonymousSubclass
	| class |
	class :=  AnonymousClassInstaller make: [ :builder |
		builder
			superclass: self theNonMetaClass  ;
			layoutClass: self theNonMetaClass classLayout class ].
	^class class.
		
