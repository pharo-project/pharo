accessing
extractAttributesFromNames
	"derive values for the receiver's style(italic), weight, and stretch inst vars.
	Also set the familyGroupName and styleNameExtracted"

	| p |

	p:= FreeTypeNameParser new
		familyNameIn: self validFamilyName;
		styleNameIn: self validStyleName;
		italicFlag: italic;
		boldFlag: bold;
		parse.
	familyGroupName := p familyName.
	slant := p extractedSlant.
	slantValue := p extractedSlantValue.
	weight := p extractedWeight.
	weightValue := p extractedWeightValue.
	stretch := p extractedStretch.
	stretchValue := p extractedStretchValue.
	upright := p extractedUpright.
	styleNameExtracted := ''.
	stretch ifNotNil:[
		styleNameExtracted := styleNameExtracted ,stretch].
	(weight notNil "and:[weight asLowercase ~= 'medium']") 
		ifTrue:[
			styleNameExtracted := styleNameExtracted , ' ', weight].
	slant ifNotNil:[
		styleNameExtracted := styleNameExtracted , ' ', slant].
	styleNameExtracted := styleNameExtracted trimBoth.
	styleNameExtracted ifEmpty: [
		styleNameExtracted := upright ifNil:['Regular']].
		 
	