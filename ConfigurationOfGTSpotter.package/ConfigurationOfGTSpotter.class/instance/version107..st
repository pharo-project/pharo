versions
version107: spec
	<version: '1.0.7' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec author: 'AndreiChis'.
		spec timestamp: '01/16/2015 19:13'.
		spec project: 'GlamourCore' with: '3.0.10'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.86';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.37' ].
