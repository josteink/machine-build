#r "D:\Git\workspacer\src\workspacer\bin\Debug\workspacer.Shared.dll"
#r "D:\Git\workspacer\src\workspacer\bin\Debug\plugins\workspacer.Bar\workspacer.Bar.dll"
#r "D:\Git\workspacer\src\workspacer\bin\Debug\plugins\workspacer.ActionMenu\workspacer.ActionMenu.dll"
#r "D:\Git\workspacer\src\workspacer\bin\Debug\plugins\workspacer.FocusIndicator\workspacer.FocusIndicator.dll"

using System;
using workspacer;
using workspacer.Bar;
using workspacer.ActionMenu;
using workspacer.FocusIndicator;

Action<IConfigContext> doConfig = (context) =>
{
    context.AddBar();
    context.AddFocusIndicator();
    var actionMenu = context.AddActionMenu();

    context.WorkspaceContainer.CreateWorkspaces("1: web", "2: terminals", "3: code", "4: files", "5: mail", "6: chat", "seven", "eight", "nine", "ten");

    var alt = KeyModifiers.Alt;
    var shift = KeyModifiers.Shift;
    var win = KeyModifiers.Control;
    Action<KeyModifiers, Keys> unsub = (mod, keys) => context.Keybinds.Unsubscribe(mod, keys);

    // unbind browser navigation keys
    unsub(alt, Keys.Right);
    unsub(alt, Keys.Left);

    // unbind default monitor keys
    unsub(alt, Keys.W);
    unsub(alt, Keys.E);
    unsub(alt, Keys.R);

    // unbind & rebind workspace keys
    unsub(alt, Keys.D1);
    unsub(alt, Keys.D2);
    unsub(alt, Keys.D3);
    unsub(alt, Keys.D4);
    unsub(alt, Keys.D5);
    unsub(alt, Keys.D6);
    unsub(alt, Keys.D7);
    unsub(alt, Keys.D8);
    unsub(alt, Keys.D9);
    unsub(alt, Keys.D0);

    unsub(alt|shift, Keys.D1);
    unsub(alt|shift, Keys.D2);
    unsub(alt|shift, Keys.D3);
    unsub(alt|shift, Keys.D4);
    unsub(alt|shift, Keys.D5);
    unsub(alt|shift, Keys.D6);
    unsub(alt|shift, Keys.D7);
    unsub(alt|shift, Keys.D8);
    unsub(alt|shift, Keys.D9);
    unsub(alt|shift, Keys.D0);

    context.Keybinds.Subscribe(win, Keys.D1, () => context.Workspaces.SwitchToWorkspace(0), "switch to workspace 1");
    context.Keybinds.Subscribe(win, Keys.D2, () => context.Workspaces.SwitchToWorkspace(1), "switch to workspace 2");
    context.Keybinds.Subscribe(win, Keys.D3, () => context.Workspaces.SwitchToWorkspace(2), "switch to workspace 3");
    context.Keybinds.Subscribe(win, Keys.D4, () => context.Workspaces.SwitchToWorkspace(3), "switch to workspace 4");
    context.Keybinds.Subscribe(win, Keys.D5, () => context.Workspaces.SwitchToWorkspace(4), "switch to workspace 5");
    context.Keybinds.Subscribe(win, Keys.D6, () => context.Workspaces.SwitchToWorkspace(5), "switch to workspace 6");
    context.Keybinds.Subscribe(win, Keys.D7, () => context.Workspaces.SwitchToWorkspace(6), "switch to workspace 7");
    context.Keybinds.Subscribe(win, Keys.D8, () => context.Workspaces.SwitchToWorkspace(7), "switch to workspace 8");
    context.Keybinds.Subscribe(win, Keys.D9, () => context.Workspaces.SwitchToWorkspace(8), "switch to workspace 9");
    context.Keybinds.Subscribe(win, Keys.D0, () => context.Workspaces.SwitchToWorkspace(9), "switch to workspace 10");

    context.Keybinds.Subscribe(win|shift, Keys.D1, () => context.Workspaces.MoveFocusedWindowToWorkspace(0), "switch focused window to workspace 1");
    context.Keybinds.Subscribe(win|shift, Keys.D2, () => context.Workspaces.MoveFocusedWindowToWorkspace(1), "switch focused window to workspace 2");
    context.Keybinds.Subscribe(win|shift, Keys.D3, () => context.Workspaces.MoveFocusedWindowToWorkspace(2), "switch focused window to workspace 3");
    context.Keybinds.Subscribe(win|shift, Keys.D4, () => context.Workspaces.MoveFocusedWindowToWorkspace(3), "switch focused window to workspace 4");
    context.Keybinds.Subscribe(win|shift, Keys.D5, () => context.Workspaces.MoveFocusedWindowToWorkspace(4), "switch focused window to workspace 5");
    context.Keybinds.Subscribe(win|shift, Keys.D6, () => context.Workspaces.MoveFocusedWindowToWorkspace(5), "switch focused window to workspace 6");
    context.Keybinds.Subscribe(win|shift, Keys.D7, () => context.Workspaces.MoveFocusedWindowToWorkspace(6), "switch focused window to workspace 7");
    context.Keybinds.Subscribe(win|shift, Keys.D8, () => context.Workspaces.MoveFocusedWindowToWorkspace(7), "switch focused window to workspace 8");
    context.Keybinds.Subscribe(win|shift, Keys.D9, () => context.Workspaces.MoveFocusedWindowToWorkspace(8), "switch focused window to workspace 9");
    context.Keybinds.Subscribe(win|shift, Keys.D0, () => context.Workspaces.MoveFocusedWindowToWorkspace(9), "switch focused window to workspace 10");
};
return doConfig;
