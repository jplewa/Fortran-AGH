using Gadfly: plot, draw, PNG, push_theme, set_default_plot_size, Geom, style, pt, cm, Scale
using DataFrames: DataFrame, vcat

function draw_plots()
    ticks = parse(Float64, readline(open("./out/results/ticks")))
    N = map(x -> parse(Int64,x), readlines(open("./out/results/N")))
    df = DataFrame(N = N, 
        time = map(x -> Float64(parse(Int64,split(x)[2])-parse(Int64,split(x)[1]))/ticks, readlines(open("./out/results/mm1"))),
        mtype="mm1")
    df = vcat(df, DataFrame(N = N, 
        time = map(x -> Float64(parse(Int64,split(x)[2])-parse(Int64,split(x)[1]))/ticks, readlines(open("./out/results/mm2"))),
        mtype="mm2"))
    df = vcat(df, DataFrame(N = N, 
        time = map(x -> Float64(parse(Int64,split(x)[2])-parse(Int64,split(x)[1]))/ticks, readlines(open("./out/results/matmul"))),
        mtype="matmul"))

    Gadfly.push_theme(:dark)
    set_default_plot_size(30cm, 30cm)
    draw(PNG("./out/plots/plot.png", 30cm, 30cm), plot(df, x="N", y="time", color="mtype", Geom.line,
            style(major_label_font_size=14pt, minor_label_font_size=12pt),
            Scale.x_log10,Scale.y_log10))
end

draw_plots()