using Gadfly: plot, draw, PNG, push_theme, set_default_plot_size, Geom, style, pt, cm, Scale
using DataFrames: DataFrame, vcat

function draw_plots()
    ticks = parse(Float64, readline(open("./out/results/ticks")))
    N = map(x -> parse(Int64,x), readlines(open("./out/results/N")))
    df = DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/mm1"))),
        mtype="naive")
    df = vcat(df, DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/mm2"))),
        mtype="dot"))
    df = vcat(df, DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/mm3"))),
        mtype="ichunk"))
    df = vcat(df, DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/mm4"))),
        mtype="ichunk+dot"))
    df = vcat(df, DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/matmul"))),
        mtype="matmul"))
    Gadfly.push_theme(:dark)
    #set_default_plot_size(30cm, 30cm)
    draw(PNG("./out/plots/plot.png", 25cm, 25cm, dpi=300), plot(df, x="N", y="time", color="mtype", Geom.point,
            style(major_label_font_size=16pt, minor_label_font_size=16pt, 
            point_label_font_size=16pt, key_title_font_size=16pt, point_size=8pt),
            Scale.x_log2,Scale.y_log2))
end

draw_plots()
