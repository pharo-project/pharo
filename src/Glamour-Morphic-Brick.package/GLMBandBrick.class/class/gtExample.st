examples
gtExample
	<gtExample>
	<label: 'Example with subbricks'>
	
	^ self new
		hSpaceFill;
		vSpaceFill;
		with: [ :b | |count|
			count := 10.
			1 to: count do: [ :index | 
				b addBrickBack: (
					GLMLabelBrick new
						text: (index asString, ' of ', count asString);
						textPosition: #center;
						color: Color random muchLighter;
						height: 40;
						hSpaceFill ) ] ]