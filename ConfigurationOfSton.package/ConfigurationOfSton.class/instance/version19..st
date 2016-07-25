versions
version19: spec
	<version: '0.19' imports: #('0.3-baseline')>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-PavelKrivanek.73';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.64' ]