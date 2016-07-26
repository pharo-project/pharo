testing
definesClass: aClass 
	^ (super definesClass: aClass) and: [ categories includes: aClass theNonMetaClass category ]