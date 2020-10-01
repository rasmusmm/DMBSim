﻿namespace DMBSim

open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Components.Hosts
open Avalonia.Threading
open System
type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "DMBSim"
        base.Width <- 1200.0
        base.Height <- 800.0
        
        
        let timer (_state: Main.State) =
            let sub (dispatch: Main.Msg -> unit) =
                let invoke() =
                    Grid.Step |> Main.GridMsg |> dispatch
                    true
                    
                DispatcherTimer.Run(Func<bool>(invoke), TimeSpan.FromMilliseconds 1000.0) |> ignore
                
            Cmd.ofSub sub

        Elmish.Program.mkProgram Main.initialState Main.update Main.view
        |> Program.withHost this
        |> Program.withSubscription timer
        |> Program.run

        
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
        this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)