accessing
downloadPharoLogo
	^ ZnClient new beOneShot
		get: 'http://pharo.org/files/pharo.png';
		entity