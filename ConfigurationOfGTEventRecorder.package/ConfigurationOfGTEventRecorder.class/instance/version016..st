versions
version016: spec
	<version: '0.16' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.22 (case 17948)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/28/2016 16:25'.
		spec project: 'Ston' with: '0.19'.
		spec 
			package: #'GT-EventRecorder' with: 'GT-EventRecorder-PavelKrivanek.66';
			package: 'GT-EventRecorder-Tests' with: 'GT-EventRecorder-Tests-JurajKubelka.37'. ].
