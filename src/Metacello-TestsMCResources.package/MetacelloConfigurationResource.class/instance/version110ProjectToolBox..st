ConfigurationOfProjectToolBox
version110ProjectToolBox: spec
	<version: '1.1.0-baseline'>

	spec for: #'common' do: [
		spec 
			package: 'Example-Core';
			package: 'Example-Base';
			package: 'Example-Test1' with: [
				spec 
					requires: #('Example-Base' );
					includes: #('Example-Core' ). ];
			package: 'Example-Test2' with: [
				spec 
					includes: #('Example-Core' );
					supplyingAnswers: #( #( 'captionMatch' 'answer' ) ). ];
			package: 'Example-Test3' with: [
				spec 
					requires: #('Example-Base' );
					supplyingAnswers: #( #( 'captionMatch' 'answer' ) ). ];
			package: 'Example-Test4' with: [
				spec 
					requires: #('Example-Base' );
					includes: #('Example-Core' );
					supplyingAnswers: #( #( 'captionMatch' 'answer' ) ). ]. ].
