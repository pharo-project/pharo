accessing
tabLineDashes
	^ tabLineDashes ifNil: [tabLineDashes := {self tabLineWidth. self tabLineWidth*3}].
