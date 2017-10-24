baselines
baseline07: spec 
	<version: '0.7-baseline'>
	spec
		for: #common
		do: [spec blessing: #baseline.
			spec repository: 'http://www.example.com/Project'.
			spec 
				package: 'Project-Core' with: [
					spec requires: 'Example-Core' ];
				package: 'Project-Tests' with: [
					spec requires: #('Project-Core' 'Example-Tests' )];
				package: 'Example-Core' with: [
					spec repository: 'http://www.example.com/Example'];
				package: 'Example-Tests' with: [
					spec requires: 'Example-Core';
						 repository: 'http://www.example.com/Example'];
				package: 'Example-AddOn' with: [
					spec requires: 'Example-Core';
						 repository: 'http://www.example.com/Example']]