system integration
systemClassReorganizedActionFrom: ann
	"when a class is reorganized, we have to check if an extension has not been added"
	| clazz classOrganization |

	clazz := ann classReorganized.
	classOrganization := clazz organization.
	classOrganization categories do: [ :each | 
		(each beginsWith: '*')
			ifTrue:  [ self checkPackageExistsOrRegister: each allButFirst ] ]
