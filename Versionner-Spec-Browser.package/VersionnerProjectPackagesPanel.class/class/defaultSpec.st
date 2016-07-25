spec
defaultSpec
	<spec>

	"Define the layout of the Project Packages Panel"
	^ SpecLayout composed
		"newColumn: [ :c1 | c1
				newRow: [:r1 | r1
						add: #packagesLabel; 
						add: #addPackageButton;
						add: #removePackageButton;
						add: #editPackageButton;
						add: #savePackageButton]
					height: 25;
				add: #packages
		];"
	"Use the code below because Spec parser takes too much time to load Versionner with the previous declaration."
		add: #packagesLabel origin: 0@0 corner: 1/4@0 offsetOrigin: 0@0 offsetCorner: 0@25;
		add: #addPackageButton origin: 1/4@0 corner: 2/4@0 offsetOrigin: 0@0 offsetCorner: 0@25;
		add: #removePackageButton origin: 2/4@0 corner: 3/4@0 offsetOrigin: 0@0 offsetCorner: 0@25;
		add: #editPackageButton origin: 3/4@0 corner: 4/4@0 offsetOrigin: 0@0 offsetCorner: 0@25;
		"add: #savePackageButton origin: 4/6@0 corner: 5/6@0 offsetOrigin: 0@0 offsetCorner: 0@25;"
		add: #packagesList origin: 0@0 corner: 1@1 offsetOrigin: 0@25 offsetCorner: 0@0;
	yourself.
