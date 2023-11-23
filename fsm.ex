defmodule FSM do
  @behaviour :gen_statem

  def init([]) do
    {:ok, :initial, 0}
  end

  def callback_mode(), do: :state_functions

  def terminate(reason, state, data) do
    IO.puts("Terminating")
    IO.inspect({reason, state, data})
  end

  def start do
    :gen_statem.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def run do
    :gen_statem.call(__MODULE__, :header)
    :gen_statem.call(__MODULE__, :data)
    :gen_statem.call(__MODULE__, :trailer)
  end

  def initial({:call, from}, :header, data) do
    reply_action = {:reply, from, data}
    IO.puts("received a header while in initial sate")

    {:next_state, :reading, data + 1, reply_action}
  end

  def initial(event_type, event_content, data) do
    handle_common(event_type, event_type, :initial, data)
  end

  def reading({:call, from}, :data, data) do
    reply_action = {:reply, from, data}
    IO.puts("received a data while in reading sate")

    {:next_state, :reading, data + 1, reply_action}
  end

  def reading({:call, from}, :trailer, data) do
    reply_action = {:reply, from, data}

    IO.puts("received a data while in reading sate")
    IO.puts("finishing the state machine")

    {:stop_and_reply, :normal, reply_action, data + 1}
  end

  def reading(event_type, event_content, data) do
    handle_common(event_type, event_type, :reading, data)
  end

  defp handle_common(event_type, event_content, state, data) do
    {:stop, :error, "just testing some stuff"}
  end
end

FSM.start()
FSM.run()
