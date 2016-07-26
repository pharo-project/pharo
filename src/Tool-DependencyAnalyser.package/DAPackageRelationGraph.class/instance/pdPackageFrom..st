accessing
pdPackageFrom: aClass
	^ packages detect: [ :package | package rPackageSet includesClass: aClass ]