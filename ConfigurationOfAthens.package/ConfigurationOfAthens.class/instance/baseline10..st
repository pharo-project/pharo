baselines
baseline10: spec 
        <version: '1.0-baseline'>
        
        spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.squeaksource.com/Athens'. 

		spec project: 'ConfigurationOfNativeBoost default' with: [
			spec
				className: 'ConfigurationOfNativeBoost';
				versionString: #'stable';
				loads: #('default' ); "this is optional"
				file: 'NativeBoost-Installer';
				repository: 'http://www.squeaksource.com/NativeBoost' ].

		spec package: 'Athens-Core' .
	
		spec package: 'Athens-Text'  with: [
			spec requires: #('Athens-Core') ].
			
		spec package: 'Athens-Balloon'  with: [
			spec requires: #('Athens-Text') ].
		
		spec package: 'Athens-Morphic'  with: [
			spec requires: #('Athens-Balloon') ].

		spec package: 'Athens-CairoPools'.
	
		spec package: 'Athens-Cairo'  with: [
			spec requires: #( 'ConfigurationOfNativeBoost default' 'Athens-Core' 'Athens-CairoPools' ) ].
	
		spec package: 'Athens-PangoCairo'  with: [
			spec requires: #( 'Athens-Cairo' ) ].
		
		spec package: 'Athens-Examples' with: [
			spec requires: #('Athens-Morphic')].
			
		spec
			group: 'Core' with: #('Athens-Core' );
			group: 'default' with: #('Athens-Morphic' 'Athens-PangoCairo' 'Athens-Examples')
	].

