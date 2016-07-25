versions
version318: spec
	<version: '3.1.8' imports: #('3.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Fixed cycle issue in Glamour

----
I try to load GT tools to minimal Pharo image (with Morphic) and I have problems to do that because it has a dependency cycle it these methods:

  GLMBrickDarkThemer>>#actionThemer
  GLMBrickThemer>>#actionThemer

These methods try to extend classes in Glamour-Morphic-Brick package but they belong to Glamour-Core package that is prerequisite of the first package. 
-----'.
		spec author: 'AndreiChis'.
		spec timestamp: '7/3/2015 13:51'.
		spec project: 'Rubric' with: '1.14'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-AndreiChis.39';
			package: 'Glamour-Core' with: 'Glamour-Core-AndreiChis.302';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.177';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-TorstenBergmann.109';
			package: 'Glamour-Examples' with: 'Glamour-Examples-CyrilFerlicot.292';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AndreiChis.400';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-AliakseiSyrel.13';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-BernardoContreras.105';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.3';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-AndreiChis.216';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-FranckWarlouzet.178';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-usmanbhatti.323';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-AndreiChis.103';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-AndreiChis.124';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-CyrilFerlicot.43';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.14'. ].
