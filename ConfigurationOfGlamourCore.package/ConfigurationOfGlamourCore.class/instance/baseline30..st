baselines
baseline30: spec
	<version: '3.0-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec description: 'Core version of Glamour that can directly be integrated in Pharo 4'.
		spec repository: 'http://www.smalltalkhub.com/mc/Moose/Glamour/main'.
		spec 
			package: 'Glamour-Announcements' ;
			package: 'Glamour-Helpers';
			package: 'Glamour-Core' with: [spec requires: #('Glamour-Helpers' 'Glamour-Announcements')];
			package: 'Glamour-Presentations' with: [spec requires: #('Glamour-Core')];
			package: 'Glamour-Rubric-Presentations' with: [spec requires: #('Rubric' 'Glamour-Presentations')];
			package: 'Glamour-Browsers' with: [spec requires: #('Glamour-Core')];
			package: 'Glamour-Examples' with: [spec requires: #('Glamour-Core')];
			package: 'Glamour-Morphic-Brick' with: [spec requires: #('Glamour-Core')];
			package: 'Glamour-Morphic-Widgets' with: [spec requires: #('Rubric' 'Glamour-Morphic-Brick')];
			package: 'Glamour-Morphic-Renderer' with: [spec requires: #('Glamour-Morphic-Widgets' 'Glamour-Core')];
			package: 'Glamour-Morphic-Pager' with: [spec requires: #('Glamour-Morphic-Renderer' 'Glamour-Morphic-Brick')];
			package: 'Glamour-Morphic-Theme' with: [spec requires: 'Rubric'];
			package: 'Glamour-Tests-Core';
			package: 'Glamour-Tests-Morphic' with: [spec requires: #('Glamour-Morphic-Renderer' 'Glamour-Morphic-Brick' 'Glamour-Morphic-Pager')];
			package: 'Glamour-Tests-Rubric' with: [spec requires: #('Glamour-Tests-Morphic' 'Glamour-Rubric-Presentations')];
			package: 'Glamour-Tests-Resources'.
			
		spec group: 'Core' with: #(
						'Glamour-Announcements'
						'Glamour-Helpers'
						'Glamour-Core'
						'Glamour-Presentations'
						'Glamour-Browsers'
						'Glamour-Examples'
						'Glamour-Rubric-Presentations').
		spec group: 'Morphic' with: #(
						'Glamour-Morphic-Brick'
						'Glamour-Morphic-Widgets'
						'Glamour-Morphic-Renderer'
						'Glamour-Morphic-Theme'
						'Glamour-Morphic-Pager').		
		spec group: 'Tests' with: #(
						'Glamour-Tests-Core'
						'Glamour-Tests-Morphic'
						'Glamour-Tests-Rubric'
						'Glamour-Tests-Resources'). 
						
		spec group: 'default' with: #(
						'Core'
						'Morphic'
						'Tests').		
		spec project: 'Rubric' with: [ 
			spec 
					className: 'ConfigurationOfRubric';
					file: 'ConfigurationOfRubric';
					version: #development;
					repository: 'http://www.smalltalkhub.com/mc/AlainPlantec/Rubric/main' ] ].