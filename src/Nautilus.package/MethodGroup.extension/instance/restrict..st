*Nautilus
restrict: anEnvironment
	^ anEnvironment selectMethods:[:m | self methods includes: m]