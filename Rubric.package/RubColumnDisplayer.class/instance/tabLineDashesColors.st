accessing
tabLineDashesColors 
	^ tabLineDashesColors ifNil: [tabLineDashesColors := {(self tabLineColor). (Color transparent)}].
