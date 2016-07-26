versions
version07: spec
	<version: '0.7' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-DamienCassou.46';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.42' ]