support
isDefinition: firstString equivalentTo: secondString 
	"When a class definition is reconstructed with #fatDefForClass, it may 
	contain extra trailing space characters in parts of the definition. This 
	is probably a minor bug, but it should be overlooked for purposes of 
	testing the change set update mechanism. The expedient here is to just 
	remove spaces before comparing the definition strings."

	^ firstString notNil
		and: [(firstString copyReplaceAll: ' ''' with: '''')
				= (secondString copyReplaceAll: ' ''' with: '''')]