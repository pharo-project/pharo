testing
includesClass: aClass 
	^ (super includesClass: aClass) and: [ categories includes: aClass theNonMetaClass category ]