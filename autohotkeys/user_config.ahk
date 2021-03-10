; Config for https://github.com/pmb6tz/windows-desktop-switcher
; and other extra config for AutoHotKeys

; === INSTRUCTIONS ===
; ====================
; 1. Any lines starting with ; are ignored
; 2. After changing this config file run script file "desktop_switcher.ahk"
; 3. Every line is in the format HOTKEY::ACTION
; ===========================
; === END OF INSTRUCTIONS ===
; ===========================

SetTitleMatchMode, RegEx

Delete & 1::
switchDesktopByNumber(1)
WinActivateBottomOnCurrentVirtualDesktop()
Return
Delete & 2::
switchDesktopByNumber(2)
WinActivateBottomOnCurrentVirtualDesktop()
Return
Delete & 3::
switchDesktopByNumber(3)
WinActivateBottomOnCurrentVirtualDesktop()
Return
Delete & 4::
switchDesktopByNumber(4)
WinActivateBottomOnCurrentVirtualDesktop()
Return
Delete & 5::
switchDesktopByNumber(5)
WinActivateBottomOnCurrentVirtualDesktop()
Return
Delete & 6::
switchDesktopByNumber(6)
WinActivateBottomOnCurrentVirtualDesktop()
Return
Delete & 7::
switchDesktopByNumber(7)
WinActivateBottomOnCurrentVirtualDesktop()
Return
Delete & 8::
switchDesktopByNumber(8)
WinActivateBottomOnCurrentVirtualDesktop()
Return
Delete & 9::
switchDesktopByNumber(9)
WinActivateBottomOnCurrentVirtualDesktop()
Return

Delete & l::switchDesktopToRight()
Delete & h::switchDesktopToLeft()
Delete & tab::
switchDesktopToLastOpened()
WinActivateBottomOnCurrentVirtualDesktop()
Return

; Swap CapsLock and Esc
Capslock::Esc

; Add a shortcut to cycle accross windows in the same virtual desktop
; https://superuser.com/questions/1457314/switch-between-windows-on-only-one-virtual-desktop-with-autohotkey-with-altta
Delete & j::WinActivateBottomOnCurrentVirtualDesktop()
Delete & k::WinActivateBottomOnCurrentVirtualDesktop()

WinActivateBottomOnCurrentVirtualDesktop(){
    list := ""
    LastWin := ""
    WinGet, id, list , , , Program Manager
    Loop, %id%
    {
        this_ID := id%A_Index%
        WinGetTitle, Title, ahk_id %this_ID%
        If (Title = "")
            continue
        If IsWindowOnCurrentVirtualDesktop(this_ID)
        {
            ;MsgBox, The window is "%Title%".
            LastWin := this_ID ; retrieves the bottommost matching window ID
        }
    }
    WinActivate, ahk_id %LastWin%
}


; https://autohotkey.com/boards/viewtopic.php?p=64295#p64295
; Indicates whether the provided window is on the currently active virtual desktop:

IsWindowOnCurrentVirtualDesktop(hWnd) {
    onCurrentDesktop := ""
    CLSID := "{aa509086-5ca9-4c25-8f95-589d3c07b48a}"
    IID := "{a5cd92ff-29be-454c-8d04-d82879fb3f1b}"
    IVirtualDesktopManager := ComObjCreate(CLSID, IID)
    Error := DllCall(NumGet(NumGet(IVirtualDesktopManager+0), 3*A_PtrSize), "Ptr", IVirtualDesktopManager, "Ptr", hWnd, "IntP", onCurrentDesktop)
    ObjRelease(IVirtualDesktopManager)
    if !(Error=0)
        return false, ErrorLevel := true
    return onCurrentDesktop, ErrorLevel := false
}
