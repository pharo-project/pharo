ConfigurationOfProjectToolBox
version131ProjectToolBox: spec
	<version: '1.3.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'. ].

	spec for: #'level1_1' do: [
		spec 
			package: 'Example-Core' with: [
				spec repository: 'http://www.example.com/1'. ];
			package: 'Example-Core' with: [
				spec repository: 'http://www.example.com/2'. ];
			package: 'Example-Core' with: [
				spec repository: 'http://www.example.com/3'. ].

		spec for: #'level2_1' do: [
			spec package: 'Example-Core' with: [
					spec repository: 'http://www.example.com/4'. ].

			spec for: #'level3_1' do: [
				spec package: 'Example-Core' with: [
						spec repository: 'http://www.example.com/5'. ]. ]. ].

		spec for: #'level2_2' do: [
			spec package: 'Example-Core' with: [
					spec repository: 'http://www.example.com/6'. ].

			spec for: #'level3_2' do: [
				spec package: 'Example-Core' with: [
						spec repository: 'http://www.example.com/7'. ]. ]. ]. ].
