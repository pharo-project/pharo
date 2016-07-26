ConfigurationOfProjectToolBox
version133ProjectToolBox: spec
	<version: '1.3.3-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'. ].

	spec for: #'level1_1' do: [
		spec package: 'GoferBar' with: 'GoferBar-jf.1'.

		spec for: #'level2_1' do: [
			spec package: 'GoferFoo' with: 'GoferFoo-lr.1'.

			spec for: #'level3_1' do: [
				spec package: 'GoferFaux' with: 'GoferFaux-tg.30'. ]. ].

		spec for: #'level2_2' do: [
			spec package: 'GeauxBar' with: 'GeauxBar.branch-lr.2'.

			spec for: #'level3_2' do: [
				spec package: 'GeauxBeau' with: 'GeauxBeau-dkh.55'. ]. ]. ].

	spec for: #'pharo' do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'. ].
