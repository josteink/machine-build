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

    var mod = KeyModifiers.Alt;
    Action<Keys> unsub = (keys) => context.Keybinds.Unsubscribe(mod, keys);

    // unbind browser navigation keys
    unsub(Keys.Right);
    unsub(Keys.Left);

    // unbind default monitor keys
    unsub(Keys.W);
    unsub(Keys.E);
    unsub(Keys.R);

    // unbind & rebind workspace keys
    unsub(Keys.D1);
    unsub(Keys.D2);
    unsub(Keys.D3);
    unsub(Keys.D4);
    unsub(Keys.D5);
    unsub(Keys.D6);
    unsub(Keys.D7);
    unsub(Keys.D8);
    unsub(Keys.D9);
    unsub(Keys.D0);

    var super = KeyModifiers.Control | KeyModifiers.Alt;
    context.Keybinds.Subscribe(super, Keys.D1, () => context.Workspaces.SwitchToWorkspace(0), "switch to workspace 1");
    context.Keybinds.Subscribe(super, Keys.D2, () => context.Workspaces.SwitchToWorkspace(1), "switch to workspace 2");
    context.Keybinds.Subscribe(super, Keys.D3, () => context.Workspaces.SwitchToWorkspace(2), "switch to workspace 3");
    context.Keybinds.Subscribe(super, Keys.D4, () => context.Workspaces.SwitchToWorkspace(3), "switch to workspace 4");
    context.Keybinds.Subscribe(super, Keys.D5, () => context.Workspaces.SwitchToWorkspace(4), "switch to workspace 5");
    context.Keybinds.Subscribe(super, Keys.D6, () => context.Workspaces.SwitchToWorkspace(5), "switch to workspace 6");
    context.Keybinds.Subscribe(super, Keys.D7, () => context.Workspaces.SwitchToWorkspace(6), "switch to workspace 7");
    context.Keybinds.Subscribe(super, Keys.D8, () => context.Workspaces.SwitchToWorkspace(7), "switch to workspace 8");
    context.Keybinds.Subscribe(super, Keys.D9, () => context.Workspaces.SwitchToWorkspace(8), "switch to workspace 9");
    context.Keybinds.Subscribe(super, Keys.D0, () => context.Workspaces.SwitchToWorkspace(9), "switch to workspace 10");
};
return doConfig;
