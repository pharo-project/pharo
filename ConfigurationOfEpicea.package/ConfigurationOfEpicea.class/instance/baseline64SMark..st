baselines
baseline64SMark: spec
	spec
		project: 'SMark'
		with:
			[ spec
				className: 'ConfigurationOfSMark';
				versionString: #development;
				file: 'ConfigurationOfSMark';
				repository: 'http://smalltalkhub.com/mc/StefanMarr/SMark/main' ].
"	spec
		package: 'SMark-Jenkins'
		with:
			[ spec
				repository: 'http://smalltalkhub.com/mc/ClementBera/classic-bench/main';
				requires: 'SMark' ]"