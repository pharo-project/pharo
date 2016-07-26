versions
version39: spec
	<version: '3.9' imports: #('3.3-baseline') >

	spec for: #'common' do: [
		spec description: '
	* New feature: File Out to Ombu
	* New feature: File Out to ChangeSet
	* Bug fix: Issues when importing ChangeSet
	* New feature: Test that overriding external methods in system are right
	* Bug fix: It was loosing class-side definition modifications.
	* Clean up: Removed tons of unused classes and methods.
	* New feature: Show class-side definitions in content tab.
	* Others
		'.
		spec author: 'MartinDias'.
		spec timestamp: '27/05/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.155';
			package: 'Epicea' with: 'Epicea-MartinDias.610'. ].