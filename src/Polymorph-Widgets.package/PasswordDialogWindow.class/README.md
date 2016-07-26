I'm a Dialog whose TextEditors characters are replaced by *. 

Example:
(UITheme builder openModal: 
        (PasswordDialogWindow new
                title: 'Authentification';
                textFont: StandardFonts defaultFont;
                text: 'Enter your password')) entryText explore