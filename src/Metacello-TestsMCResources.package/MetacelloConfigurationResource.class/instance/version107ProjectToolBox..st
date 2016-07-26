ConfigurationOfProjectToolBox
version107ProjectToolBox: spec
	<version: '1.0.7-baseline'>

	spec for: #'common' do: [
		spec 
			package: 'Example-Core' with: [
				spec 
					supplyingAnswers: #( #( 'preload' 'preload answer' ) #( 'postload' 'postload answer' ) );
					preLoadDoIt: #'preloadForSupplyingAnswers';
					postLoadDoIt: #'postloadForSupplyingAnswers'. ];
			package: 'Example-Tests' with: [
				spec supplyingAnswers: #( #( 'string' 'preload answer' ) #( 'symbol' #'abc def' ) #( 'integer' 1 ) #( 'boolean' true ) #( 'another boolean' false ) #( 'character' $a ) ). ]. ].
