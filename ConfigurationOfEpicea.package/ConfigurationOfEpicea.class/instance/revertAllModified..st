scripting
revertAllModified: somePackageNames

	somePackageNames 
		do: [ :name |
			(self hasModifiedMark: name)
				ifTrue: [ 
					Gofer new
						url: 'http://smalltalkhub.com/mc/Pharo/Pharo30/main';
						package: name;
						revert ] ]