versions
version320: spec
	<version: '3.2.0' imports: #('3.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Add GLMPresentation>>##hasTransformation - like #hasTitle, but for #display:'.
		spec author: 'SeanDeNigris'.
		spec timestamp: '8/22/2015 15:22'.
		spec project: 'Rubric' with: '1.18.4'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-AndreiChis.39';
			package: 'Glamour-Core' with: 'Glamour-Core-SeanDeNigris.303';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.177';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-TorstenBergmann.109';
			package: 'Glamour-Examples' with: 'Glamour-Examples-MarcusDenker.293';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AndreiChis.401';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-AliakseiSyrel.13';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-BernardoContreras.105';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.3';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-AndreiChis.216';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.180';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-MarcusDenker.326';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-AndreiChis.103';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-AndreiChis.124';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-CyrilFerlicot.43';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.14'. ].
