private
checkDataSizes

" 
VGTigerDemo checkDataSizes
"

	self assert: (self commands size = self tigerCommandCount).
	self assert: (self tigerPointsCount = self tigerPoints size).