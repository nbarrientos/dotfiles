// Installed add-ons:
// * Dark reader
// * ClearURLs
// * uBlock Origin
// * SwitchyOmega

// Theme: Complete Black Theme for Firefox
//        https://addons.mozilla.org/en-US/firefox/addon/complete-black-theme-for-firef/?src=turnoffthelights.com

user_pref("browser.display.background_color.dark", "#000000");
user_pref("browser.download.lastDir", "/home/nacho/Downloads");
// Never open links in tabs
user_pref("browser.link.open_newwindow", 2);
user_pref("browser.newtabpage.enabled", false);
user_pref("browser.startup.homepage", "about:blank");
// Do not open previous windows and tabs
user_pref("browser.startup.page", 1);
user_pref("browser.tabs.warnOnClose", false);
user_pref("browser.urlbar.suggest.engines", false);
user_pref("browser.urlbar.suggest.openpage", false);
user_pref("keyword.enabled", false);
user_pref("network.automatic-ntlm-auth.trusted-uris", "cern.ch");
user_pref("network.negotiate-auth.trusted-uris", "cern.ch");
user_pref("permissions.default.desktop-notification", 2);
user_pref("permissions.default.geo", 2);
user_pref("security.OCSP.enabled", 1);
user_pref("signon.rememberSignons", false);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
// Do not bring up the menu bar when alt is kept pressed
user_pref("ui.key.menuAccessKeyFocuses", false);
user_pref("ui.textSelectAttentionBackground", "#d0bc00");
user_pref("ui.textHighlightBackground", "#79a8ff");
