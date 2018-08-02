using Gadfly: plot, draw, PNG, push_theme, set_default_plot_size, Geom, style, pt, cm, Scale
using DataFrames: DataFrame, vcat

function draw_plots()
    ticks = parse(Float64, readline(open("./out/results/ticks")))
    N = map(x -> parse(Int64,x), readlines(open("./out/results/N")))
    df = DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/mm1"))),
        method="naive")
    df = vcat(df, DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/mm2"))),
        method="dot"))
    df = vcat(df, DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/mm3"))),
        method="ichunk"))
    df = vcat(df, DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/mm4"))),
        method="ichunk+dot"))
    df = vcat(df, DataFrame(N = N,
        time = map(x -> Float64(parse(Int64,x))/ticks, readlines(open("./out/results/matmul"))),
        method="matmul"))
    Gadfly.push_theme(:dark)
    set_default_plot_size(30cm, 30cm)
    draw(PNG("./out/plots/plot.png", 30cm, 30cm), plot(df, x="N", y="time", color="method", Geom.point,
            style(major_label_font_size=14pt, minor_label_font_size=10pt, 
            point_label_font_size=16pt, key_title_font_size=16pt), shape="method",
            Scale.x_log10,Scale.y_log10))
end

draw_plots()
