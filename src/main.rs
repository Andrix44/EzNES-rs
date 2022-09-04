#![allow(non_snake_case)]

use imgui::{self, MenuItem};
use glium::{glutin, Surface};
use imgui_glium_renderer::Renderer;
//use winit;
use imgui_winit_support::WinitPlatform;


fn init() -> (glutin::event_loop::EventLoop<()>, glium::Display, imgui::Context, WinitPlatform, Renderer) {
    let event_loop = glutin::event_loop::EventLoop::new();
    let context = glutin::ContextBuilder::new().with_vsync(true);
    let builder = glutin::window::WindowBuilder::new().with_title("EzNES-2").with_inner_size(glutin::dpi::LogicalSize::new(1280f64, 720f64));
    let display = glium::Display::new(builder, context, &event_loop).expect("Failed to create display!");

    let mut ctx = imgui::Context::create();
    let mut platform = WinitPlatform::init(&mut ctx);
    {
        let gl_window = display.gl_window();
        let window = gl_window.window();
    
        platform.attach_window(
          ctx.io_mut(),
          window,
          imgui_winit_support::HiDpiMode::Default,
        );
    }

    let renderer = Renderer::init(&mut ctx, &display).expect("Failed to create renderer!");

    (event_loop, display, ctx, platform, renderer)
}

struct State {
    pub log: bool,
    pub debug: bool,
    pub demo: bool,
    pub pattern_table: bool,
    pub palette: bool,
    pub nametable: bool,
    pub multiplayer: bool,
}

fn main() {
    let (event_loop, display, mut ctx, mut platform, mut renderer) = init();

    let mut state = State{
        log: false,
        debug: false,
        demo: false,
        pattern_table: false,
        palette: false,
        nametable: false,
        multiplayer: false,
    };

    let mut last_frame = std::time::Instant::now();
    use glutin::event::Event::{NewEvents, MainEventsCleared, RedrawRequested, WindowEvent};
    event_loop.run(move |event, _target, control_flow| match event {
        NewEvents(_) => {
            ctx.io_mut().update_delta_time(last_frame.elapsed());
            last_frame = std::time::Instant::now();
        }

        MainEventsCleared => {
            let gl_window = display.gl_window();
            platform.prepare_frame(ctx.io_mut(), gl_window.window()).expect("Failed to init frame!");
            gl_window.window().request_redraw();
        }

        RedrawRequested(_) => {
            let ui = ctx.frame();
            let gl_window = display.gl_window();
            let mut target = display.draw();
            target.clear_color(0f32, 0f32, 0f32, 255f32);

            {
                if let Some(main_menu) = ui.begin_main_menu_bar() {
                    if let Some(file_menu) = ui.begin_menu("File") {
                        if MenuItem::new("Load ROM").build(&ui) {
                            // TODO: load ROM
                        }

                        if MenuItem::new("Exit").build(&ui) {
                            *control_flow = glutin::event_loop::ControlFlow::Exit;
                        }

                        file_menu.end();
                    }

                    if let Some(emulation_menu) = ui.begin_menu("Emulation") {
                        ui.checkbox("Enable second controller", &mut state.multiplayer);
                        emulation_menu.end();
                    }

                    if let Some(window_menu) = ui.begin_menu("Window") {
                        ui.checkbox("Show log", &mut state.log);
                        ui.checkbox("Show debug window", &mut state.debug);
                        ui.checkbox("Show demo window", &mut state.demo);
                        ui.checkbox("Show pattern table", &mut state.pattern_table);
                        ui.checkbox("Show palette", &mut state.palette);
                        ui.checkbox("Show nametables", &mut state.nametable);
                        
                        window_menu.end();
                    }

                    main_menu.end();
                }
            }

            if state.demo {
                ui.show_demo_window(&mut state.demo);
            }

            platform.prepare_render(&ui, gl_window.window());
            let draw_data = ui.render();
            renderer.render(&mut target, draw_data).expect("Failed to render UI!");
            target.finish().expect("Failed to swap the window frame!");
        }

        WindowEvent {
            window_id: _,
            event: glutin::event::WindowEvent::CloseRequested
        } => {
            *control_flow = glutin::event_loop::ControlFlow::Exit;
        }

        event => {
            let gl_window = display.gl_window();
            platform.handle_event(ctx.io_mut(), gl_window.window(), &event);
        }
    });
}

//https://github.com/shmolyneaux/imgui_gfx_example/blob/master/src/main.rs - maybe useful
