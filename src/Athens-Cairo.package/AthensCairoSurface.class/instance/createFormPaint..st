paints
createFormPaint: aForm 
	"here we should convert form to cairo surface"
	
	| newSurface |

	newSurface := self class fromForm: (aForm asFormOfDepth: 32).
	
	^ newSurface asAthensPaintOn: context

