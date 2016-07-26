known names
weightNames
	"Answer a sequence of arrays.
	Each array has an integer weight value as its first element.
	The remaining elements are String tokens which might appear
	within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #weightNames put: nil.
	"
	weightNames ifNotNil:[^weightNames].
	^weightNames := {
		{LogicalFont weightExtraThin. 'extra'. 'thin'}.
		{LogicalFont weightExtraThin.'extrathin'}.
		{LogicalFont weightExtraThin. 'ext'. 'thin'}.
		{LogicalFont weightExtraThin. 'extthin'}.
		{LogicalFont weightUltraThin.'ultra'. 'thin'}.
		{LogicalFont weightUltraThin.'ultrathin'}.
		{LogicalFont weightExtraLight. 'extra'. 'light'}.
		{LogicalFont weightExtraLight. 'extralight'}.
		{LogicalFont weightExtraLight. 'ext'. 'light'}.
		{LogicalFont weightExtraLight. 'extlight'}.
		{LogicalFont weightUltraLight. 'ultra'. 'light'}.
		{LogicalFont weightUltraLight. 'ultralight'}.
		{LogicalFont weightSemiBold. 'semi'. 'bold'}.
		{LogicalFont weightSemiBold. 'semibold'}.
		{LogicalFont weightDemiBold. 'demi'. 'bold'}.
		{LogicalFont weightDemiBold. 'demibold'}.
		{LogicalFont weightExtraBold. 'extra'. 'bold'}.
		{LogicalFont weightExtraBold. 'extrabold'}.
		{LogicalFont weightExtraBold. 'ext'. 'bold'}.
		{LogicalFont weightExtraBold. 'extbold'}. 
		{LogicalFont weightUltraBold. 'ultra'. 'bold'}.
		{LogicalFont weightUltraBold. 'ultrabold'}.
		{LogicalFont weightExtraBlack. 'extra'. 'black'}.
		{LogicalFont weightExtraBlack. 'extrablack'}.
		{LogicalFont weightExtraBlack. 'ext'. 'black'}.
		{LogicalFont weightExtraBlack. 'extblack'}.
		{LogicalFont weightUltraBlack.'ultra'. 'black'}.
		{LogicalFont weightUltraBlack. 'ultrablack'}.
		{LogicalFont weightBold. 'bold'}.
		{LogicalFont weightThin.'thin'}.
		{LogicalFont weightLight. 'light'}.
		{LogicalFont weightMedium. 'medium'}.
		{LogicalFont weightBlack. 'black'}.
		{LogicalFont weightHeavy. 'heavy'}.
		{LogicalFont weightNord. 'nord'}.
		{LogicalFont weightDemi. 'demi'}.
		{LogicalFont weightUltra. 'ultra'}.
		} 
			