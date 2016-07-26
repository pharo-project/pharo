versions
version017: spec
	<version: '0.17' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'sync with Pharo packages'.
		spec author: 'PavelKrivanek'.
		spec timestamp: '6/1/2016 11:40'.
		spec project: 'Ston' with: '0.19'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-TheIntegrator.68';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.37'. ].
